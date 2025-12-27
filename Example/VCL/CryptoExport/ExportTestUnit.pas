unit ExportTestUnit;

{$IFDEF FPC}
{$MODE DELPHI}
{$ENDIF}
{$H+}

interface

procedure RunAll;

implementation

uses
  SysUtils, Classes, CnCryptoExport;

procedure Print(const S: string);
begin
  WriteLn(S);
end;

procedure Check(B: Boolean; const Name: string; var Passed: Integer; var Failed: Integer);
begin
  if B then
  begin
    Inc(Passed);
    Print('PASS ' + Name);
  end
  else
  begin
    Inc(Failed);
    Print('FAIL ' + Name);
  end;
end;

function BytesEqual(A, B: Pointer; L: TCnSize): Boolean;
begin
  if L = 0 then
    begin
      Result := True;
      Exit;
    end;
  Result := CompareMem(A, B, L);
end;

procedure RunAll;
var
  Passed, Failed: Integer;
  Mi: TInt32;
  Mj, Mn, Pt: TUInt32;
  Mu: TUInt64;
  R: TCnResult;
  P: TCnCryptoHandle;
  OutLen: TCnSize;
  Buf: array[0..255] of Byte;
  Hex: array[0..255] of Byte;
  Decoded: array[0..255] of Byte;
  Digest: array[0..63] of Byte;
  Mac: array[0..63] of Byte;
  Key: array[0..31] of Byte;
  Iv: array[0..31] of Byte;
  InBlock: array[0..255] of Byte;
  OutBlock: array[0..4095] of Byte;
  PlainBack: array[0..4095] of Byte;
  Tag: array[0..31] of Byte;
  TagLen: TCnSize;
  CipherLen: TCnSize;
  Share: array[0..63] of Byte;
  ShareLen: TCnSize;
  Cipher: array[0..4095] of Byte;
  EncLen, DecLen: TCnSize;
  Priv, Pub: TCnCryptoHandle;
  ModBytes: TCnSize;
  RSABlock: array[0..8191] of Byte;
  RSALen: TCnSize;
  Hotp: array[0..31] of Byte;
  Totp: array[0..31] of Byte;
  Ctx: array[0..63] of Byte;
begin
  Passed := 0;
  Failed := 0;

  R := cn_get_version(Mj, Mn, Pt);
  Check((R = CN_OK) and (Mj >= 0), 'cn_get_version', Passed, Failed);
  Check(cn_get_abi_version >= 3, 'cn_get_abi_version', Passed, Failed);

  P := cn_alloc(16);
  Check(P <> nil, 'cn_alloc', Passed, Failed);
  R := cn_memzero(P, 16);
  Check(R = CN_OK, 'cn_memzero', Passed, Failed);
  R := cn_free(P);
  Check(R = CN_OK, 'cn_free', Passed, Failed);

  Check((cn_endian_is_le + cn_endian_is_be) = 1, 'cn_endian', Passed, Failed);

  Move('abc'[1], InBlock[0], 3 * SizeOf(Char));
  R := cn_data_to_hex(@InBlock[0], 3 * SizeOf(Char), @Hex[0], SizeOf(Hex), OutLen);
  Check((R = CN_OK) and (OutLen = 6 * SizeOf(Char)), 'cn_data_to_hex', Passed, Failed);

  R := cn_base64_encode(@InBlock[0], 3 * SizeOf(Char), @Buf[0], SizeOf(Buf), OutLen);
  Check((R = CN_OK) and (OutLen > 0), 'cn_base64_encode', Passed, Failed);
  R := cn_base64_decode(@Buf[0], OutLen, @Decoded[0], SizeOf(Decoded), OutLen);
  Check((R = CN_OK) and (OutLen = 3 * SizeOf(Char)) and BytesEqual(@Decoded[0], @InBlock[0], 3 * SizeOf(Char)), 'cn_base64_decode', Passed, Failed);

  R := cn_base64url_encode(@InBlock[0], 3 * SizeOf(Char), @Buf[0], SizeOf(Buf), OutLen);
  Check((R = CN_OK) and (OutLen > 0), 'cn_base64url_encode', Passed, Failed);
  R := cn_base64url_decode(@Buf[0], OutLen, @Decoded[0], SizeOf(Decoded), OutLen);
  Check((R = CN_OK) and (OutLen = 3 * SizeOf(Char)) and BytesEqual(@Decoded[0], @InBlock[0], 3 * SizeOf(Char)), 'cn_base64url_decode', Passed, Failed);

  R := cn_hash_digest(CN_HASH_SHA2_256, @InBlock[0], 3, @Digest[0], SizeOf(Digest), OutLen);
  Check((R = CN_OK) and (OutLen = 32), 'cn_hash_digest_sha256', Passed, Failed);

  Move('key'[1], Key[0], 3);
  R := cn_hmac(CN_HASH_SHA2_256, @Key[0], 3, @InBlock[0], 3, @Mac[0], SizeOf(Mac), OutLen);
  Check((R = CN_OK) and (OutLen = 32), 'cn_hmac_sha256', Passed, Failed);

  R := cn_kdf_pbkdf2(CN_HASH_SHA2_256, @Key[0], 3, @InBlock[0], 3, 1000, @Buf[0], SizeOf(Buf), OutLen);
  Check((R = CN_OK) and (OutLen > 0), 'cn_kdf_pbkdf2', Passed, Failed);
  R := cn_kdf_hkdf(CN_HASH_SHA2_256, @Key[0], 3, @Iv[0], 3, @InBlock[0], 3, 32, @Buf[0], SizeOf(Buf), OutLen);
  Check((R = CN_OK) and (OutLen = 32), 'cn_kdf_hkdf', Passed, Failed);

  FillChar(Key[0], 16, 0);
  FillChar(InBlock[0], 16, 0);
  R := cn_cipher_encrypt(CN_CIPHER_AES128_ECB, @Key[0], 16, nil, 0, @InBlock[0], 16, @OutBlock[0], SizeOf(OutBlock), OutLen);
  Check((R = CN_OK) and (OutLen = 16), 'cn_cipher_encrypt_aes128_ecb', Passed, Failed);
  R := cn_cipher_decrypt(CN_CIPHER_AES128_ECB, @Key[0], 16, nil, 0, @OutBlock[0], OutLen, @PlainBack[0], SizeOf(PlainBack), OutLen);
  Check((R = CN_OK) and (OutLen = 16) and BytesEqual(@PlainBack[0], @InBlock[0], 16), 'cn_cipher_decrypt_aes128_ecb', Passed, Failed);

  FillChar(Key[0], 16, 1);
  FillChar(Iv[0], 12, 2);
  FillChar(InBlock[0], 16, 3);
  R := cn_aead_encrypt(CN_AEAD_AES128_GCM, @Key[0], 16, @Iv[0], 12, nil, 0, @InBlock[0], 16, @OutBlock[0], SizeOf(OutBlock), CipherLen, @Tag[0], SizeOf(Tag), TagLen);
  Check((R = CN_OK) and (CipherLen = 16) and (TagLen = CN_AEAD_TAG_BYTES), 'cn_aead_encrypt_aes128_gcm', Passed, Failed);
  R := cn_aead_decrypt(CN_AEAD_AES128_GCM, @Key[0], 16, @Iv[0], 12, nil, 0, @OutBlock[0], CipherLen, @Tag[0], TagLen, @PlainBack[0], SizeOf(PlainBack), OutLen);
  Check((R = CN_OK) and (OutLen = 16) and BytesEqual(@PlainBack[0], @InBlock[0], 16), 'cn_aead_decrypt_aes128_gcm', Passed, Failed);

  FillChar(Key[0], 16, 31);
  FillChar(Iv[0], 12, 32);
  FillChar(InBlock[0], 16, 33);
  Move('aad!'[1], Ctx[0], 4);
  R := cn_aead_encrypt(CN_AEAD_AES128_GCM, @Key[0], 16, @Iv[0], 12, @Ctx[0], 4, @InBlock[0], 16, @OutBlock[0], SizeOf(OutBlock), CipherLen, @Tag[0], SizeOf(Tag), TagLen);
  Check((R = CN_OK) and (CipherLen = 16) and (TagLen = CN_AEAD_TAG_BYTES), 'cn_aead_encrypt_aes128_gcm_aad', Passed, Failed);
  R := cn_aead_decrypt(CN_AEAD_AES128_GCM, @Key[0], 16, @Iv[0], 12, @Ctx[0], 4, @OutBlock[0], CipherLen, @Tag[0], TagLen, @PlainBack[0], SizeOf(PlainBack), OutLen);
  Check((R = CN_OK) and (OutLen = 16) and BytesEqual(@PlainBack[0], @InBlock[0], 16), 'cn_aead_decrypt_aes128_gcm_aad', Passed, Failed);

  FillChar(Key[0], 24, 4);
  FillChar(Iv[0], 12, 5);
  FillChar(InBlock[0], 32, 6);
  R := cn_aead_encrypt(CN_AEAD_AES192_GCM, @Key[0], 24, @Iv[0], 12, nil, 0, @InBlock[0], 32, @OutBlock[0], SizeOf(OutBlock), CipherLen, @Tag[0], SizeOf(Tag), TagLen);
  Check((R = CN_OK) and (TagLen = CN_AEAD_TAG_BYTES), 'cn_aead_encrypt_aes192_gcm', Passed, Failed);
  R := cn_aead_decrypt(CN_AEAD_AES192_GCM, @Key[0], 24, @Iv[0], 12, nil, 0, @OutBlock[0], CipherLen, @Tag[0], TagLen, @PlainBack[0], SizeOf(PlainBack), OutLen);
  Check((R = CN_OK) and BytesEqual(@PlainBack[0], @InBlock[0], OutLen), 'cn_aead_decrypt_aes192_gcm', Passed, Failed);

  FillChar(Key[0], 32, 7);
  FillChar(Iv[0], 12, 8);
  FillChar(InBlock[0], 32, 9);
  R := cn_aead_encrypt(CN_AEAD_AES256_GCM, @Key[0], 32, @Iv[0], 12, nil, 0, @InBlock[0], 32, @OutBlock[0], SizeOf(OutBlock), CipherLen, @Tag[0], SizeOf(Tag), TagLen);
  Check((R = CN_OK) and (TagLen = CN_AEAD_TAG_BYTES), 'cn_aead_encrypt_aes256_gcm', Passed, Failed);
  R := cn_aead_decrypt(CN_AEAD_AES256_GCM, @Key[0], 32, @Iv[0], 12, nil, 0, @OutBlock[0], CipherLen, @Tag[0], TagLen, @PlainBack[0], SizeOf(PlainBack), OutLen);
  Check((R = CN_OK) and BytesEqual(@PlainBack[0], @InBlock[0], OutLen), 'cn_aead_decrypt_aes256_gcm', Passed, Failed);

  FillChar(Key[0], 24, 10);
  FillChar(InBlock[0], 16, 11);
  R := cn_cipher_encrypt(CN_CIPHER_AES192_ECB, @Key[0], 24, nil, 0, @InBlock[0], 16, @OutBlock[0], SizeOf(OutBlock), OutLen);
  Check((R = CN_OK) and (OutLen = 16), 'cn_cipher_encrypt_aes192_ecb', Passed, Failed);
  R := cn_cipher_decrypt(CN_CIPHER_AES192_ECB, @Key[0], 24, nil, 0, @OutBlock[0], OutLen, @PlainBack[0], SizeOf(PlainBack), OutLen);
  Check((R = CN_OK) and (OutLen = 16) and BytesEqual(@PlainBack[0], @InBlock[0], 16), 'cn_cipher_decrypt_aes192_ecb', Passed, Failed);

  FillChar(Key[0], 32, 12);
  FillChar(InBlock[0], 16, 13);
  R := cn_cipher_encrypt(CN_CIPHER_AES256_ECB, @Key[0], 32, nil, 0, @InBlock[0], 16, @OutBlock[0], SizeOf(OutBlock), OutLen);
  Check((R = CN_OK) and (OutLen = 16), 'cn_cipher_encrypt_aes256_ecb', Passed, Failed);
  R := cn_cipher_decrypt(CN_CIPHER_AES256_ECB, @Key[0], 32, nil, 0, @OutBlock[0], OutLen, @PlainBack[0], SizeOf(PlainBack), OutLen);
  Check((R = CN_OK) and (OutLen = 16) and BytesEqual(@PlainBack[0], @InBlock[0], 16), 'cn_cipher_decrypt_aes256_ecb', Passed, Failed);

  FillChar(Key[0], 16, 34);
  FillChar(Iv[0], 16, 35);
  FillChar(InBlock[0], 16, 36);
  R := cn_cipher_encrypt(CN_CIPHER_AES128_OFB, @Key[0], 16, @Iv[0], 16, @InBlock[0], 16, @OutBlock[0], SizeOf(OutBlock), OutLen);
  Check(R = CN_OK, 'cn_cipher_encrypt_aes128_ofb', Passed, Failed);
  R := cn_cipher_decrypt(CN_CIPHER_AES128_OFB, @Key[0], 16, @Iv[0], 16, @OutBlock[0], OutLen, @PlainBack[0], SizeOf(PlainBack), OutLen);
  Check((R = CN_OK) and BytesEqual(@PlainBack[0], @InBlock[0], OutLen), 'cn_cipher_decrypt_aes128_ofb', Passed, Failed);

  FillChar(Key[0], 16, 37);
  FillChar(Iv[0], 16, 38);
  FillChar(InBlock[0], 16, 39);
  R := cn_cipher_encrypt(CN_CIPHER_AES128_CFB, @Key[0], 16, @Iv[0], 16, @InBlock[0], 16, @OutBlock[0], SizeOf(OutBlock), OutLen);
  Check(R = CN_OK, 'cn_cipher_encrypt_aes128_cfb', Passed, Failed);
  R := cn_cipher_decrypt(CN_CIPHER_AES128_CFB, @Key[0], 16, @Iv[0], 16, @OutBlock[0], OutLen, @PlainBack[0], SizeOf(PlainBack), OutLen);
  Check((R = CN_OK) and BytesEqual(@PlainBack[0], @InBlock[0], OutLen), 'cn_cipher_decrypt_aes128_cfb', Passed, Failed);

  FillChar(Key[0], 16, 40);
  FillChar(Iv[0], 16, 41);
  FillChar(InBlock[0], 32, 42);
  R := cn_cipher_encrypt(CN_CIPHER_AES128_CTR, @Key[0], 16, @Iv[0], 16, @InBlock[0], 32, @OutBlock[0], SizeOf(OutBlock), OutLen);
  Check(R = CN_OK, 'cn_cipher_encrypt_aes128_ctr', Passed, Failed);
  R := cn_cipher_decrypt(CN_CIPHER_AES128_CTR, @Key[0], 16, @Iv[0], 16, @OutBlock[0], OutLen, @PlainBack[0], SizeOf(PlainBack), OutLen);
  Check((R = CN_OK) and BytesEqual(@PlainBack[0], @InBlock[0], OutLen), 'cn_cipher_decrypt_aes128_ctr', Passed, Failed);

  FillChar(Key[0], 8, 43);
  FillChar(Iv[0], 8, 44);
  FillChar(InBlock[0], 16, 45);
  R := cn_cipher_encrypt(CN_CIPHER_DES_ECB, @Key[0], 8, nil, 0, @InBlock[0], 16, @OutBlock[0], SizeOf(OutBlock), OutLen);
  Check((R = CN_OK) and (OutLen = 16), 'cn_cipher_encrypt_des_ecb', Passed, Failed);
  R := cn_cipher_decrypt(CN_CIPHER_DES_ECB, @Key[0], 8, nil, 0, @OutBlock[0], OutLen, @PlainBack[0], SizeOf(PlainBack), OutLen);
  Check((R = CN_OK) and (OutLen = 16) and BytesEqual(@PlainBack[0], @InBlock[0], 16), 'cn_cipher_decrypt_des_ecb', Passed, Failed);

  FillChar(Key[0], 8, 46);
  FillChar(Iv[0], 8, 47);
  FillChar(InBlock[0], 16, 48);
  R := cn_cipher_encrypt(CN_CIPHER_DES_CBC, @Key[0], 8, @Iv[0], 8, @InBlock[0], 16, @OutBlock[0], SizeOf(OutBlock), OutLen);
  Check((R = CN_OK) and (OutLen = 16), 'cn_cipher_encrypt_des_cbc', Passed, Failed);
  R := cn_cipher_decrypt(CN_CIPHER_DES_CBC, @Key[0], 8, @Iv[0], 8, @OutBlock[0], OutLen, @PlainBack[0], SizeOf(PlainBack), OutLen);
  Check((R = CN_OK) and (OutLen = 16) and BytesEqual(@PlainBack[0], @InBlock[0], 16), 'cn_cipher_decrypt_des_cbc', Passed, Failed);

  FillChar(Key[0], 24, 49);
  FillChar(InBlock[0], 16, 50);
  R := cn_cipher_encrypt(CN_CIPHER_3DES_ECB, @Key[0], 24, nil, 0, @InBlock[0], 16, @OutBlock[0], SizeOf(OutBlock), OutLen);
  Check((R = CN_OK) and (OutLen = 16), 'cn_cipher_encrypt_3des_ecb', Passed, Failed);
  R := cn_cipher_decrypt(CN_CIPHER_3DES_ECB, @Key[0], 24, nil, 0, @OutBlock[0], OutLen, @PlainBack[0], SizeOf(PlainBack), OutLen);
  Check((R = CN_OK) and (OutLen = 16) and BytesEqual(@PlainBack[0], @InBlock[0], 16), 'cn_cipher_decrypt_3des_ecb', Passed, Failed);

  FillChar(Key[0], 24, 51);
  FillChar(Iv[0], 8, 52);
  FillChar(InBlock[0], 16, 53);
  R := cn_cipher_encrypt(CN_CIPHER_3DES_CBC, @Key[0], 24, @Iv[0], 8, @InBlock[0], 16, @OutBlock[0], SizeOf(OutBlock), OutLen);
  Check((R = CN_OK) and (OutLen = 16), 'cn_cipher_encrypt_3des_cbc', Passed, Failed);
  R := cn_cipher_decrypt(CN_CIPHER_3DES_CBC, @Key[0], 24, @Iv[0], 8, @OutBlock[0], OutLen, @PlainBack[0], SizeOf(PlainBack), OutLen);
  Check((R = CN_OK) and (OutLen = 16) and BytesEqual(@PlainBack[0], @InBlock[0], 16), 'cn_cipher_decrypt_3des_cbc', Passed, Failed);

  R := cn_hash_digest(CN_HASH_BLAKE224, @InBlock[0], 3, @Digest[0], SizeOf(Digest), OutLen);
  Check((R = CN_OK) and (OutLen = 28), 'cn_hash_digest_blake224', Passed, Failed);
  R := cn_hash_digest(CN_HASH_BLAKE256, @InBlock[0], 3, @Digest[0], SizeOf(Digest), OutLen);
  Check((R = CN_OK) and (OutLen = 32), 'cn_hash_digest_blake256', Passed, Failed);
  R := cn_hash_digest(CN_HASH_BLAKE384, @InBlock[0], 3, @Digest[0], SizeOf(Digest), OutLen);
  Check((R = CN_OK) and (OutLen = 48), 'cn_hash_digest_blake384', Passed, Failed);
  R := cn_hash_digest(CN_HASH_BLAKE512, @InBlock[0], 3, @Digest[0], SizeOf(Digest), OutLen);
  Check((R = CN_OK) and (OutLen = 64), 'cn_hash_digest_blake512', Passed, Failed);

  Move('key2'[1], Key[0], 4);
  R := cn_hmac(CN_HASH_BLAKE2S, @Key[0], 4, @InBlock[0], 3, @Mac[0], SizeOf(Mac), OutLen);
  Check((R = CN_OK) and (OutLen = 32), 'cn_hmac_blake2s', Passed, Failed);
  R := cn_hmac(CN_HASH_BLAKE2B, @Key[0], 4, @InBlock[0], 3, @Mac[0], SizeOf(Mac), OutLen);
  Check((R = CN_OK) and (OutLen = 64), 'cn_hmac_blake2b', Passed, Failed);

  R := cn_hash_digest(CN_HASH_XXH32, @InBlock[0], 3, @Digest[0], SizeOf(Digest), OutLen);
  Check((R = CN_OK) and (OutLen = 4), 'cn_hash_digest_xxh32', Passed, Failed);
  R := cn_hash_digest(CN_HASH_XXH64, @InBlock[0], 3, @Digest[0], SizeOf(Digest), OutLen);
  Check((R = CN_OK) and (OutLen = 8), 'cn_hash_digest_xxh64', Passed, Failed);

  R := cn_rsa_generate_keys(1024, 1, Priv, Pub, 0);
  Check(R = CN_OK, 'cn_rsa_generate_keys_pkcs1', Passed, Failed);
  Move('abc'[1], InBlock[0], 3);
  R := cn_rsa_encrypt_with_public(CN_RSA_PAD_PKCS1, Pub, @InBlock[0], 3, @RSABlock[0], SizeOf(RSABlock), RSALen);
  Check(R = CN_OK, 'cn_rsa_encrypt_with_public_pkcs1', Passed, Failed);
  R := cn_rsa_decrypt_with_private(CN_RSA_PAD_PKCS1, Priv, @RSABlock[0], RSALen, @PlainBack[0], SizeOf(PlainBack), OutLen);
  Check((R = CN_OK) and (OutLen = 3) and BytesEqual(@PlainBack[0], @InBlock[0], 3), 'cn_rsa_decrypt_with_private_pkcs1', Passed, Failed);
  R := cn_rsa_encrypt_with_private(Priv, @InBlock[0], 3, @RSABlock[0], SizeOf(RSABlock), RSALen);
  Check(R = CN_OK, 'cn_rsa_encrypt_with_private', Passed, Failed);
  R := cn_rsa_decrypt_with_public(Pub, @RSABlock[0], RSALen, @PlainBack[0], SizeOf(PlainBack), OutLen);
  Check((R = CN_OK) and (OutLen = 3) and BytesEqual(@PlainBack[0], @InBlock[0], 3), 'cn_rsa_decrypt_with_public', Passed, Failed);
  R := cn_rsa_key_free(Pub);
  Check(R = CN_OK, 'cn_rsa_key_free_pub_pkcs1', Passed, Failed);
  R := cn_rsa_key_free(Priv);
  Check(R = CN_OK, 'cn_rsa_key_free_priv_pkcs1', Passed, Failed);

  R := cn_rsa_generate_keys(1024, 1, Priv, Pub, 0);
  Check(R = CN_OK, 'cn_rsa_generate_keys', Passed, Failed);
  Move('abc'[1], InBlock[0], 3);
  R := cn_rsa_encrypt_with_public(CN_RSA_PAD_OAEP, Pub, @InBlock[0], 3, @RSABlock[0], SizeOf(RSABlock), RSALen);
  Check(R = CN_OK, 'cn_rsa_encrypt_with_public', Passed, Failed);
  R := cn_rsa_decrypt_with_private(CN_RSA_PAD_OAEP, Priv, @RSABlock[0], RSALen, @PlainBack[0], SizeOf(PlainBack), OutLen);
  Check((R = CN_OK) and (OutLen = 3) and BytesEqual(@PlainBack[0], @InBlock[0], 3), 'cn_rsa_decrypt_with_private', Passed, Failed);
  ModBytes := cn_rsa_pubkey_get_modulus_bytes(Pub);
  Check(ModBytes > 0, 'cn_rsa_pubkey_get_modulus_bytes', Passed, Failed);
  ModBytes := cn_rsa_privkey_get_modulus_bytes(Priv);
  Check(ModBytes > 0, 'cn_rsa_privkey_get_modulus_bytes', Passed, Failed);
  R := cn_rsa_key_free(Pub);
  Check(R = CN_OK, 'cn_rsa_key_free_pub', Passed, Failed);
  R := cn_rsa_key_free(Priv);
  Check(R = CN_OK, 'cn_rsa_key_free_priv', Passed, Failed);

  FillChar(Key[0], 32, 7);
  R := cn_otp_hotp(@Key[0], 32, 1, 6, @Hotp[0], SizeOf(Hotp), OutLen);
  Check((R = CN_OK) and (OutLen = 6), 'cn_otp_hotp', Passed, Failed);
  R := cn_otp_totp(CN_HASH_SHA2_256, @Key[0], 32, 30, 6, @Totp[0], SizeOf(Totp), OutLen);
  Check((R = CN_OK) and (OutLen = 6), 'cn_otp_totp', Passed, Failed);

  R := cn_const_time_equal(@InBlock[0], @InBlock[0], 3);
  Check(R = 1, 'cn_const_time_equal', Passed, Failed);
  R := cn_const_time_select(1, @InBlock[0], @OutBlock[0], 3, @PlainBack[0]);
  Check(BytesEqual(@PlainBack[0], @InBlock[0], 3), 'cn_const_time_select', Passed, Failed);

  R := cn_str_to_uint64(@Hex[0], 1 * SizeOf(Char), Mu);
  Check((R = CN_OK) and (Mu <> 0), 'cn_str_to_uint64', Passed, Failed);

  ShareLen := 32;
  R := cn_mlkem_expected_encap_key_len(CN_MLKEM_TYPE_512);
  Check(R > 0, 'cn_mlkem_expected_encap_key_len', Passed, Failed);
  R := cn_mlkem_share_key_bytes;
  Check(R = 32, 'cn_mlkem_share_key_bytes', Passed, Failed);
  //R := cn_selftest_mlkem(CN_MLKEM_TYPE_512);
  //Check(R = CN_OK, 'cn_selftest_mlkem_512', Passed, Failed);
  //R := cn_selftest_mlkem(CN_MLKEM_TYPE_768);
  //Check(R = CN_OK, 'cn_selftest_mlkem_768', Passed, Failed);
  //R := cn_selftest_mlkem(CN_MLKEM_TYPE_1024);
  //Check(R = CN_OK, 'cn_selftest_mlkem_1024', Passed, Failed);

  R := cn_mldsa_expected_privkey_len(CN_MLDSA_TYPE_44);
  Check(R > 0, 'cn_mldsa_expected_privkey_len', Passed, Failed);
  //R := cn_selftest_mldsa(CN_MLDSA_TYPE_44, CN_HASH_SHA2_256);
  //Check(R = CN_OK, 'cn_selftest_mldsa_44_sha256', Passed, Failed);
  //R := cn_selftest_mldsa(CN_MLDSA_TYPE_65, CN_HASH_SHAKE128);
  //Check(R = CN_OK, 'cn_selftest_mldsa_65_shake128', Passed, Failed);
  //R := cn_selftest_mldsa(CN_MLDSA_TYPE_87, CN_HASH_SHAKE256);
  //Check(R = CN_OK, 'cn_selftest_mldsa_87_shake256', Passed, Failed);

  R := cn_hash_digest(CN_HASH_SHA3_256, @InBlock[0], 3, @Digest[0], SizeOf(Digest), OutLen);
  Check((R = CN_OK) and (OutLen = 32), 'cn_hash_digest_sha3_256', Passed, Failed);
  R := cn_hash_digest(CN_HASH_SM3, @InBlock[0], 3, @Digest[0], SizeOf(Digest), OutLen);
  Check((R = CN_OK) and (OutLen = 32), 'cn_hash_digest_sm3', Passed, Failed);
  Move('key'[1], Key[0], 3);
  R := cn_hmac(CN_HASH_SM3, @Key[0], 3, @InBlock[0], 3, @Mac[0], SizeOf(Mac), OutLen);
  Check((R = CN_OK) and (OutLen = 32), 'cn_hmac_sm3', Passed, Failed);

  FillChar(Key[0], 16, 0);
  FillChar(Iv[0], 16, 1);
  FillChar(InBlock[0], 16, 2);
  R := cn_cipher_encrypt(CN_CIPHER_AES256_CBC, @Key[0], 32, @Iv[0], 16, @InBlock[0], 16, @OutBlock[0], SizeOf(OutBlock), OutLen);
  Check((R = CN_OK) and (OutLen = 16), 'cn_cipher_encrypt_aes256_cbc', Passed, Failed);
  R := cn_cipher_decrypt(CN_CIPHER_AES256_CBC, @Key[0], 32, @Iv[0], 16, @OutBlock[0], OutLen, @PlainBack[0], SizeOf(PlainBack), OutLen);
  Check((R = CN_OK) and (OutLen = 16) and BytesEqual(@PlainBack[0], @InBlock[0], 16), 'cn_cipher_decrypt_aes256_cbc', Passed, Failed);

  FillChar(Key[0], 16, 3);
  FillChar(Iv[0], 16, 4);
  FillChar(InBlock[0], 16, 5);
  R := cn_cipher_encrypt(CN_CIPHER_SM4_ECB, @Key[0], 16, nil, 0, @InBlock[0], 16, @OutBlock[0], SizeOf(OutBlock), OutLen);
  Check((R = CN_OK) and (OutLen = 16), 'cn_cipher_encrypt_sm4_ecb', Passed, Failed);
  R := cn_cipher_decrypt(CN_CIPHER_SM4_ECB, @Key[0], 16, nil, 0, @OutBlock[0], OutLen, @PlainBack[0], SizeOf(PlainBack), OutLen);
  Check((R = CN_OK) and (OutLen = 16) and BytesEqual(@PlainBack[0], @InBlock[0], 16), 'cn_cipher_decrypt_sm4_ecb', Passed, Failed);

  FillChar(Key[0], 16, 6);
  FillChar(Iv[0], 16, 7);
  FillChar(InBlock[0], 16, 8);
  R := cn_cipher_encrypt(CN_CIPHER_SM4_CBC, @Key[0], 16, @Iv[0], 16, @InBlock[0], 16, @OutBlock[0], SizeOf(OutBlock), OutLen);
  Check((R = CN_OK) and (OutLen = 16), 'cn_cipher_encrypt_sm4_cbc', Passed, Failed);
  R := cn_cipher_decrypt(CN_CIPHER_SM4_CBC, @Key[0], 16, @Iv[0], 16, @OutBlock[0], OutLen, @PlainBack[0], SizeOf(PlainBack), OutLen);
  Check((R = CN_OK) and (OutLen = 16) and BytesEqual(@PlainBack[0], @InBlock[0], 16), 'cn_cipher_decrypt_sm4_cbc', Passed, Failed);

  FillChar(Key[0], 16, 9);
  FillChar(Iv[0], 16, 10);
  FillChar(InBlock[0], 16, 11);
  R := cn_cipher_encrypt(CN_CIPHER_SM4_CFB, @Key[0], 16, @Iv[0], 16, @InBlock[0], 16, @OutBlock[0], SizeOf(OutBlock), OutLen);
  Check(R = CN_OK, 'cn_cipher_encrypt_sm4_cfb', Passed, Failed);
  R := cn_cipher_decrypt(CN_CIPHER_SM4_CFB, @Key[0], 16, @Iv[0], 16, @OutBlock[0], OutLen, @PlainBack[0], SizeOf(PlainBack), OutLen);
  Check((R = CN_OK) and BytesEqual(@PlainBack[0], @InBlock[0], OutLen), 'cn_cipher_decrypt_sm4_cfb', Passed, Failed);

  FillChar(Key[0], 16, 12);
  FillChar(Iv[0], 16, 13);
  FillChar(InBlock[0], 16, 14);
  R := cn_cipher_encrypt(CN_CIPHER_SM4_OFB, @Key[0], 16, @Iv[0], 16, @InBlock[0], 16, @OutBlock[0], SizeOf(OutBlock), OutLen);
  Check(R = CN_OK, 'cn_cipher_encrypt_sm4_ofb', Passed, Failed);
  R := cn_cipher_decrypt(CN_CIPHER_SM4_OFB, @Key[0], 16, @Iv[0], 16, @OutBlock[0], OutLen, @PlainBack[0], SizeOf(PlainBack), OutLen);
  Check((R = CN_OK) and BytesEqual(@PlainBack[0], @InBlock[0], OutLen), 'cn_cipher_decrypt_sm4_ofb', Passed, Failed);

  FillChar(Key[0], 16, 15);
  FillChar(Iv[0], 16, 16);
  FillChar(InBlock[0], 32, 17);
  R := cn_cipher_encrypt(CN_CIPHER_SM4_CTR, @Key[0], 16, @Iv[0], 16, @InBlock[0], 32, @OutBlock[0], SizeOf(OutBlock), OutLen);
  Check(R = CN_OK, 'cn_cipher_encrypt_sm4_ctr', Passed, Failed);
  R := cn_cipher_decrypt(CN_CIPHER_SM4_CTR, @Key[0], 16, @Iv[0], 16, @OutBlock[0], OutLen, @PlainBack[0], SizeOf(PlainBack), OutLen);
  Check((R = CN_OK) and BytesEqual(@PlainBack[0], @InBlock[0], OutLen), 'cn_cipher_decrypt_sm4_ctr', Passed, Failed);

  FillChar(Key[0], 16, 18);
  FillChar(InBlock[0], 32, 19);
  R := cn_cipher_encrypt(CN_CIPHER_RC4, @Key[0], 16, nil, 0, @InBlock[0], 32, @OutBlock[0], SizeOf(OutBlock), OutLen);
  Check(R = CN_OK, 'cn_cipher_encrypt_rc4', Passed, Failed);
  R := cn_cipher_decrypt(CN_CIPHER_RC4, @Key[0], 16, nil, 0, @OutBlock[0], OutLen, @PlainBack[0], SizeOf(PlainBack), OutLen);
  Check((R = CN_OK) and BytesEqual(@PlainBack[0], @InBlock[0], OutLen), 'cn_cipher_decrypt_rc4', Passed, Failed);

  FillChar(Key[0], 32, 20);
  FillChar(Iv[0], 12, 21);
  FillChar(InBlock[0], 64, 22);
  R := cn_aead_encrypt(CN_AEAD_CHACHA20_POLY1305, @Key[0], 32, @Iv[0], 12, nil, 0, @InBlock[0], 64, @OutBlock[0], SizeOf(OutBlock), CipherLen, @Tag[0], SizeOf(Tag), TagLen);
  Check((R = CN_OK) and (TagLen = CN_AEAD_TAG_BYTES), 'cn_aead_encrypt_chacha20_poly1305', Passed, Failed);
  R := cn_aead_decrypt(CN_AEAD_CHACHA20_POLY1305, @Key[0], 32, @Iv[0], 12, nil, 0, @OutBlock[0], CipherLen, @Tag[0], TagLen, @PlainBack[0], SizeOf(PlainBack), OutLen);
  Check((R = CN_OK) and BytesEqual(@PlainBack[0], @InBlock[0], OutLen), 'cn_aead_decrypt_chacha20_poly1305', Passed, Failed);

  FillChar(Key[0], 32, 23);
  FillChar(Iv[0], 24, 24);
  FillChar(InBlock[0], 64, 25);
  R := cn_aead_encrypt(CN_AEAD_XCHACHA20_POLY1305, @Key[0], 32, @Iv[0], 24, nil, 0, @InBlock[0], 64, @OutBlock[0], SizeOf(OutBlock), CipherLen, @Tag[0], SizeOf(Tag), TagLen);
  Check((R = CN_OK) and (TagLen = CN_AEAD_TAG_BYTES), 'cn_aead_encrypt_xchacha20_poly1305', Passed, Failed);
  R := cn_aead_decrypt(CN_AEAD_XCHACHA20_POLY1305, @Key[0], 32, @Iv[0], 24, nil, 0, @OutBlock[0], CipherLen, @Tag[0], TagLen, @PlainBack[0], SizeOf(PlainBack), OutLen);
  Check((R = CN_OK) and BytesEqual(@PlainBack[0], @InBlock[0], OutLen), 'cn_aead_decrypt_xchacha20_poly1305', Passed, Failed);

  FillChar(Key[0], 16, 26);
  FillChar(Iv[0], 12, 27);
  FillChar(InBlock[0], 64, 28);
  R := cn_aead_encrypt(CN_AEAD_SM4_GCM, @Key[0], 16, @Iv[0], 12, nil, 0, @InBlock[0], 64, @OutBlock[0], SizeOf(OutBlock), CipherLen, @Tag[0], SizeOf(Tag), TagLen);
  Check((R = CN_OK) and (TagLen = CN_AEAD_TAG_BYTES), 'cn_aead_encrypt_sm4_gcm', Passed, Failed);
  R := cn_aead_decrypt(CN_AEAD_SM4_GCM, @Key[0], 16, @Iv[0], 12, nil, 0, @OutBlock[0], CipherLen, @Tag[0], TagLen, @PlainBack[0], SizeOf(PlainBack), OutLen);
  Check((R = CN_OK) and BytesEqual(@PlainBack[0], @InBlock[0], OutLen), 'cn_aead_decrypt_sm4_gcm', Passed, Failed);

  R := cn_sm2_generate_keys(Priv, Pub);
  Check(R = CN_OK, 'cn_sm2_generate_keys', Passed, Failed);
  Move('1234567812345678'[1], Ctx[0], 16);
  Move('sm2data'[1], InBlock[0], 7);
  R := cn_sm2_sign(@Ctx[0], 16, Priv, Pub, @InBlock[0], 7, @OutBlock[0], SizeOf(OutBlock), OutLen);
  Check((R = CN_OK) and (OutLen > 0), 'cn_sm2_sign', Passed, Failed);
  R := cn_sm2_verify(@Ctx[0], 16, Pub, @InBlock[0], 7, @OutBlock[0], OutLen);
  Check(R = 1, 'cn_sm2_verify', Passed, Failed);
  R := cn_sm2_encrypt(CN_SM2_SEQ_C1C3C2, 1, Pub, @InBlock[0], 7, @OutBlock[0], SizeOf(OutBlock), OutLen);
  Check((R = CN_OK) and (OutLen > 0), 'cn_sm2_encrypt', Passed, Failed);
  R := cn_sm2_decrypt(CN_SM2_SEQ_C1C3C2, Priv, @OutBlock[0], OutLen, @PlainBack[0], SizeOf(PlainBack), OutLen);
  Check((R = CN_OK) and (OutLen = 7) and BytesEqual(@PlainBack[0], @InBlock[0], 7), 'cn_sm2_decrypt', Passed, Failed);
  R := cn_sm2_save_public_key_to_pem(Pub, @OutBlock[0], SizeOf(OutBlock), OutLen);
  Check((R = CN_OK) and (OutLen > 0), 'cn_sm2_save_public_key_to_pem', Passed, Failed);
  R := cn_sm2_save_keys_to_pem(Priv, Pub, @OutBlock[0], SizeOf(OutBlock), OutLen);
  Check((R = CN_OK) and (OutLen > 0), 'cn_sm2_save_keys_to_pem', Passed, Failed);
  R := cn_sm2_key_free(Pub);
  Check(R = CN_OK, 'cn_sm2_key_free_pub', Passed, Failed);
  R := cn_sm2_key_free(Priv);
  Check(R = CN_OK, 'cn_sm2_key_free_priv', Passed, Failed);

  R := cn_ed25519_generate_keys(Priv, Pub);
  Check(R = CN_OK, 'cn_ed25519_generate_keys', Passed, Failed);
  Move('edmsg'[1], InBlock[0], 5);
  R := cn_ed25519_sign(Priv, Pub, @InBlock[0], 5, @OutBlock[0], SizeOf(OutBlock), OutLen);
  Check((R = CN_OK) and (OutLen = 64), 'cn_ed25519_sign', Passed, Failed);
  R := cn_ed25519_verify(Pub, @InBlock[0], 5, @OutBlock[0], OutLen);
  Check(R = 1, 'cn_ed25519_verify', Passed, Failed);
  Move('ctx'[1], Ctx[0], 3);
  R := cn_ed25519_sign_ex(@Ctx[0], 3, 0, Priv, Pub, @InBlock[0], 5, @OutBlock[0], SizeOf(OutBlock), OutLen);
  Check((R = CN_OK) and (OutLen = 64), 'cn_ed25519_sign_ex', Passed, Failed);
  R := cn_ed25519_verify_ex(@Ctx[0], 3, 0, Pub, @InBlock[0], 5, @OutBlock[0], OutLen);
  Check(R = 1, 'cn_ed25519_verify_ex', Passed, Failed);
  R := cn_ed25519_privkey_to_bytes(Priv, @Buf[0], SizeOf(Buf), OutLen);
  Check((R = CN_OK) and (OutLen = 32), 'cn_ed25519_privkey_to_bytes', Passed, Failed);
  R := cn_ed25519_pubkey_to_bytes(Pub, @Decoded[0], SizeOf(Decoded), EncLen);
  Check((R = CN_OK) and (EncLen = 32), 'cn_ed25519_pubkey_to_bytes', Passed, Failed);
  R := cn_ed25519_key_free(Pub);
  Check(R = CN_OK, 'cn_ed25519_key_free_pub', Passed, Failed);
  R := cn_ed25519_key_free(Priv);
  Check(R = CN_OK, 'cn_ed25519_key_free_priv', Passed, Failed);
  R := cn_ed25519_privkey_from_bytes(@Buf[0], OutLen, Priv);
  Check(R = CN_OK, 'cn_ed25519_privkey_from_bytes', Passed, Failed);
  R := cn_ed25519_pubkey_from_bytes(@Decoded[0], EncLen, Pub);
  Check(R = CN_OK, 'cn_ed25519_pubkey_from_bytes', Passed, Failed);
  R := cn_ed25519_sign(Priv, Pub, @InBlock[0], 5, @OutBlock[0], SizeOf(OutBlock), OutLen);
  Check((R = CN_OK) and (OutLen = 64), 'cn_ed25519_sign_after_reload', Passed, Failed);
  R := cn_ed25519_verify(Pub, @InBlock[0], 5, @OutBlock[0], 64);
  Check(R = 1, 'cn_ed25519_verify_after_reload', Passed, Failed);
  R := cn_ed25519_key_free(Pub);
  Check(R = CN_OK, 'cn_ed25519_key_free_pub2', Passed, Failed);
  R := cn_ed25519_key_free(Priv);
  Check(R = CN_OK, 'cn_ed25519_key_free_priv2', Passed, Failed);

  R := cn_curve25519_generate_keys(Priv, Pub);
  Check(R = CN_OK, 'cn_curve25519_generate_keys', Passed, Failed);
  R := cn_curve25519_derive_public_to_bytes(Priv, @Buf[0], SizeOf(Buf), OutLen);
  Check((R = CN_OK) and (OutLen = 32), 'cn_curve25519_derive_public_to_bytes', Passed, Failed);
  R := cn_curve25519_dh_step1(Priv, @OutBlock[0], SizeOf(OutBlock), OutLen);
  Check((R = CN_OK) and (OutLen = 32), 'cn_curve25519_dh_step1_self', Passed, Failed);
  R := cn_curve25519_generate_keys(Priv, Pub);
  Check(R = CN_OK, 'cn_curve25519_generate_keys_peer', Passed, Failed);
  R := cn_curve25519_dh_step1(Priv, @Decoded[0], SizeOf(Decoded), EncLen);
  Check((R = CN_OK) and (EncLen = 32), 'cn_curve25519_dh_step1_peer', Passed, Failed);
  R := cn_curve25519_dh_bytes(@Buf[0], 32, @Decoded[0], 32, @Cipher[0], SizeOf(Cipher), RSALen);
  Check((R = CN_OK) and (RSALen = 32), 'cn_curve25519_dh_bytes', Passed, Failed);
  R := cn_curve25519_key_free(Pub);
  Check(R = CN_OK, 'cn_curve25519_key_free_pub', Passed, Failed);
  R := cn_curve25519_key_free(Priv);
  Check(R = CN_OK, 'cn_curve25519_key_free_priv', Passed, Failed);

  ShareLen := cn_mlkem_expected_encap_key_len(CN_MLKEM_TYPE_512);
  R := cn_mlkem_generate_keys(CN_MLKEM_TYPE_512, nil, 0, nil, 0, @Cipher[0], SizeOf(Cipher), EncLen, @RSABlock[0], SizeOf(RSABlock), DecLen);
  Check((R = CN_OK) and (EncLen = ShareLen) and (DecLen = cn_mlkem_expected_decap_key_len(CN_MLKEM_TYPE_512)), 'cn_mlkem_generate_keys_512', Passed, Failed);
  Move('0123456789ABCDEF0123456789ABCDEF'[1], InBlock[0], 32);
  R := cn_mlkem_encaps(CN_MLKEM_TYPE_512, @Cipher[0], EncLen, @InBlock[0], 32, @Share[0], SizeOf(Share), ShareLen, @OutBlock[0], SizeOf(OutBlock), CipherLen);
  Check((R = CN_OK) and (ShareLen = 32) and (CipherLen = cn_mlkem_expected_cipher_len(CN_MLKEM_TYPE_512)), 'cn_mlkem_encaps_512', Passed, Failed);
  R := cn_mlkem_decaps(CN_MLKEM_TYPE_512, @RSABlock[0], DecLen, @OutBlock[0], CipherLen, @Hotp[0], SizeOf(Hotp), OutLen);
  Check((R = CN_OK) and (OutLen = 32) and BytesEqual(@Hotp[0], @Share[0], 32), 'cn_mlkem_decaps_512', Passed, Failed);
  R := cn_mlkem_encaps_auto(CN_MLKEM_TYPE_512, @Cipher[0], EncLen, @Share[0], SizeOf(Share), ShareLen, @OutBlock[0], SizeOf(OutBlock), CipherLen);
  Check((R = CN_OK) and (ShareLen = 32) and (CipherLen = cn_mlkem_expected_cipher_len(CN_MLKEM_TYPE_512)), 'cn_mlkem_encaps_auto_512', Passed, Failed);
  R := cn_mlkem_decaps(CN_MLKEM_TYPE_512, @RSABlock[0], DecLen, @OutBlock[0], CipherLen, @Hotp[0], SizeOf(Hotp), OutLen);
  Check((R = CN_OK) and (OutLen = 32), 'cn_mlkem_decaps_after_auto_512', Passed, Failed);

  R := cn_mldsa_generate_keys(CN_MLDSA_TYPE_44, nil, 0, @RSABlock[0], SizeOf(RSABlock), RSALen, @Cipher[0], SizeOf(Cipher), EncLen);
  Check((R = CN_OK) and (RSALen = cn_mldsa_expected_privkey_len(CN_MLDSA_TYPE_44)) and (EncLen = cn_mldsa_expected_pubkey_len(CN_MLDSA_TYPE_44)), 'cn_mldsa_generate_keys_44', Passed, Failed);
  Move('hello'[1], InBlock[0], 5);
  R := cn_mldsa_sign(CN_MLDSA_TYPE_44, @RSABlock[0], RSALen, @InBlock[0], 5, nil, 0, CN_HASH_SHA2_256, nil, 0, @RSABlock[0], SizeOf(RSABlock), RSALen);
  Check((R = CN_OK) and (RSALen = cn_mldsa_expected_sig_len(CN_MLDSA_TYPE_44)), 'cn_mldsa_sign_44_sha256', Passed, Failed);
  R := cn_mldsa_verify(CN_MLDSA_TYPE_44, @Cipher[0], EncLen, @InBlock[0], 5, @RSABlock[0], RSALen, nil, 0, CN_HASH_SHA2_256);
  Check(R = 1, 'cn_mldsa_verify_44_sha256', Passed, Failed);

  R := cn_ecc_curve_bytes(CN_ECC_CURVE_SECP256K1);
  Check(R > 0, 'cn_ecc_curve_bytes_secp256k1', Passed, Failed);
  R := cn_ecc_generate_keys(CN_ECC_CURVE_SECP256K1, Priv, Pub);
  Check(R = CN_OK, 'cn_ecc_generate_keys_secp256k1', Passed, Failed);
  Move('abc'[1], InBlock[0], 3);
  R := cn_ecc_sign(CN_HASH_SHA2_256, CN_ECC_CURVE_SECP256K1, Priv, @InBlock[0], 3, @OutBlock[0], SizeOf(OutBlock), OutLen);
  Check((R = CN_OK) and (OutLen > 0), 'cn_ecc_sign_secp256k1', Passed, Failed);
  R := cn_ecc_verify(CN_HASH_SHA2_256, CN_ECC_CURVE_SECP256K1, Pub, @InBlock[0], 3, @OutBlock[0], OutLen);
  Check(R = 1, 'cn_ecc_verify_secp256k1', Passed, Failed);
  R := cn_ecc_save_keys_to_pem(CN_ECC_KEY_PKCS1, CN_ECC_CURVE_SECP256K1, Priv, Pub, @Cipher[0], SizeOf(Cipher), EncLen);
  Check((R = CN_OK) and (EncLen > 0), 'cn_ecc_save_keys_to_pem_secp256k1', Passed, Failed);
  R := cn_ecc_load_keys_from_pem(@Cipher[0], EncLen, nil, 0, Priv, Pub, Mi);
  Check((R = CN_OK) and (Mi = CN_ECC_CURVE_SECP256K1), 'cn_ecc_load_keys_from_pem_secp256k1', Passed, Failed);
  R := cn_ecc_save_public_key_to_pem(CN_ECC_KEY_PKCS1, CN_ECC_CURVE_SECP256K1, Pub, @Cipher[0], SizeOf(Cipher), EncLen);
  Check((R = CN_OK) and (EncLen > 0), 'cn_ecc_save_public_key_to_pem_secp256k1', Passed, Failed);
  R := cn_ecc_key_free(Pub);
  Check(R = CN_OK, 'cn_ecc_key_free_pub_secp256k1', Passed, Failed);
  R := cn_ecc_key_free(Priv);
  Check(R = CN_OK, 'cn_ecc_key_free_priv_secp256k1', Passed, Failed);

  FillChar(Key[0], 32, 77);
  FillChar(Iv[0], 12, 66);
  FillChar(InBlock[0], 64, 55);
  R := cn_cipher_encrypt(CN_CIPHER_CHACHA20, @Key[0], 32, @Iv[0], 12, @InBlock[0], 64, @OutBlock[0], SizeOf(OutBlock), OutLen);
  Check((R = CN_OK) and (OutLen = 64), 'cn_cipher_encrypt_chacha20', Passed, Failed);
  R := cn_cipher_decrypt(CN_CIPHER_CHACHA20, @Key[0], 32, @Iv[0], 12, @OutBlock[0], OutLen, @PlainBack[0], SizeOf(PlainBack), OutLen);
  Check((R = CN_OK) and BytesEqual(@PlainBack[0], @InBlock[0], OutLen), 'cn_cipher_decrypt_chacha20', Passed, Failed);

  FillChar(Key[0], 16, 88);
  FillChar(Iv[0], 16, 99);
  FillChar(InBlock[0], 64, 11);
  R := cn_cipher_encrypt(CN_CIPHER_ZUC, @Key[0], 16, @Iv[0], 16, @InBlock[0], 64, @OutBlock[0], SizeOf(OutBlock), OutLen);
  Check((R = CN_OK) and (OutLen = 64), 'cn_cipher_encrypt_zuc', Passed, Failed);
  R := cn_cipher_decrypt(CN_CIPHER_ZUC, @Key[0], 16, @Iv[0], 16, @OutBlock[0], OutLen, @PlainBack[0], SizeOf(PlainBack), OutLen);
  Check((R = CN_OK) and BytesEqual(@PlainBack[0], @InBlock[0], OutLen), 'cn_cipher_decrypt_zuc', Passed, Failed);

  R := cn_rsa_generate_keys(1024, 1, Priv, Pub, 0);
  Check(R = CN_OK, 'cn_rsa_generate_keys_for_pem', Passed, Failed);
  R := cn_rsa_save_keys_to_pem(CN_RSA_KEY_PKCS1, Priv, Pub, @Cipher[0], SizeOf(Cipher), EncLen);
  Check((R = CN_OK) and (EncLen > 0), 'cn_rsa_save_keys_to_pem_pkcs1', Passed, Failed);
  R := cn_rsa_load_keys_from_pem(@Cipher[0], EncLen, nil, 0, Priv, Pub);
  Check(R = CN_OK, 'cn_rsa_load_keys_from_pem_pkcs1', Passed, Failed);
  R := cn_rsa_save_public_key_to_pem(CN_RSA_KEY_PKCS1, Pub, @Cipher[0], SizeOf(Cipher), EncLen);
  Check((R = CN_OK) and (EncLen > 0), 'cn_rsa_save_public_key_to_pem_pkcs1', Passed, Failed);
  Move('xyz'[1], InBlock[0], 3);
  R := cn_rsa_sign(CN_HASH_SHA2_256, Priv, @InBlock[0], 3, @RSABlock[0], SizeOf(RSABlock), RSALen);
  Check((R = CN_OK) and (RSALen > 0), 'cn_rsa_sign_pkcs1', Passed, Failed);
  R := cn_rsa_verify(CN_HASH_SHA2_256, Pub, @InBlock[0], 3, @RSABlock[0], RSALen);
  Check(R = 1, 'cn_rsa_verify_pkcs1', Passed, Failed);
  R := cn_rsa_key_free(Pub);
  Check(R = CN_OK, 'cn_rsa_key_free_pub_pem', Passed, Failed);
  R := cn_rsa_key_free(Priv);
  Check(R = CN_OK, 'cn_rsa_key_free_priv_pem', Passed, Failed);

  ShareLen := cn_mlkem_expected_encap_key_len(CN_MLKEM_TYPE_768);
  R := cn_mlkem_generate_keys(CN_MLKEM_TYPE_768, nil, 0, nil, 0, @Cipher[0], SizeOf(Cipher), EncLen, @RSABlock[0], SizeOf(RSABlock), DecLen);
  Check((R = CN_OK) and (EncLen = ShareLen) and (DecLen = cn_mlkem_expected_decap_key_len(CN_MLKEM_TYPE_768)), 'cn_mlkem_generate_keys_768', Passed, Failed);
  Move('0123456789ABCDEF0123456789ABCDEF'[1], InBlock[0], 32);
  R := cn_mlkem_encaps(CN_MLKEM_TYPE_768, @Cipher[0], EncLen, @InBlock[0], 32, @Share[0], SizeOf(Share), ShareLen, @OutBlock[0], SizeOf(OutBlock), CipherLen);
  Check((R = CN_OK) and (ShareLen = 32) and (CipherLen = cn_mlkem_expected_cipher_len(CN_MLKEM_TYPE_768)), 'cn_mlkem_encaps_768', Passed, Failed);
  R := cn_mlkem_decaps(CN_MLKEM_TYPE_768, @RSABlock[0], DecLen, @OutBlock[0], CipherLen, @Hotp[0], SizeOf(Hotp), OutLen);
  Check((R = CN_OK) and (OutLen = 32) and BytesEqual(@Hotp[0], @Share[0], 32), 'cn_mlkem_decaps_768', Passed, Failed);
  R := cn_mlkem_check_encap_key(CN_MLKEM_TYPE_768, @Cipher[0], EncLen);
  Check(R = CN_OK, 'cn_mlkem_check_encap_key_768_ok', Passed, Failed);
  R := cn_mlkem_check_decap_key(CN_MLKEM_TYPE_768, @RSABlock[0], DecLen);
  Check(R = CN_OK, 'cn_mlkem_check_decap_key_768_ok', Passed, Failed);
  R := cn_mlkem_check_encap_key(CN_MLKEM_TYPE_768, @Cipher[0], EncLen - 1);
  Check(R = CN_E_INVALID_ARG, 'cn_mlkem_check_encap_key_768_badlen', Passed, Failed);
  R := cn_mlkem_check_decap_key(CN_MLKEM_TYPE_768, @RSABlock[0], DecLen - 1);
  Check(R = CN_E_INVALID_ARG, 'cn_mlkem_check_decap_key_768_badlen', Passed, Failed);

  ShareLen := cn_mlkem_expected_encap_key_len(CN_MLKEM_TYPE_1024);
  R := cn_mlkem_generate_keys(CN_MLKEM_TYPE_1024, nil, 0, nil, 0, @Cipher[0], SizeOf(Cipher), EncLen, @RSABlock[0], SizeOf(RSABlock), DecLen);
  Check((R = CN_OK) and (EncLen = ShareLen) and (DecLen = cn_mlkem_expected_decap_key_len(CN_MLKEM_TYPE_1024)), 'cn_mlkem_generate_keys_1024', Passed, Failed);
  Move('FEDCBA9876543210FEDCBA9876543210'[1], InBlock[0], 32);
  R := cn_mlkem_encaps(CN_MLKEM_TYPE_1024, @Cipher[0], EncLen, @InBlock[0], 32, @Share[0], SizeOf(Share), ShareLen, @OutBlock[0], SizeOf(OutBlock), CipherLen);
  Check((R = CN_OK) and (ShareLen = 32) and (CipherLen = cn_mlkem_expected_cipher_len(CN_MLKEM_TYPE_1024)), 'cn_mlkem_encaps_1024', Passed, Failed);
  R := cn_mlkem_decaps(CN_MLKEM_TYPE_1024, @RSABlock[0], DecLen, @OutBlock[0], CipherLen, @Hotp[0], SizeOf(Hotp), OutLen);
  Check((R = CN_OK) and (OutLen = 32) and BytesEqual(@Hotp[0], @Share[0], 32), 'cn_mlkem_decaps_1024', Passed, Failed);
  R := cn_mlkem_check_encap_key(CN_MLKEM_TYPE_1024, @Cipher[0], EncLen);
  Check(R = CN_OK, 'cn_mlkem_check_encap_key_1024_ok', Passed, Failed);
  R := cn_mlkem_check_decap_key(CN_MLKEM_TYPE_1024, @RSABlock[0], DecLen);
  Check(R = CN_OK, 'cn_mlkem_check_decap_key_1024_ok', Passed, Failed);
  R := cn_mlkem_check_encap_key(CN_MLKEM_TYPE_1024, @Cipher[0], EncLen - 1);
  Check(R = CN_E_INVALID_ARG, 'cn_mlkem_check_encap_key_1024_badlen', Passed, Failed);
  R := cn_mlkem_check_decap_key(CN_MLKEM_TYPE_1024, @RSABlock[0], DecLen - 1);
  Check(R = CN_E_INVALID_ARG, 'cn_mlkem_check_decap_key_1024_badlen', Passed, Failed);

  R := cn_mldsa_generate_keys(CN_MLDSA_TYPE_65, nil, 0, @RSABlock[0], SizeOf(RSABlock), RSALen, @Cipher[0], SizeOf(Cipher), EncLen);
  Check((R = CN_OK) and (RSALen = cn_mldsa_expected_privkey_len(CN_MLDSA_TYPE_65)) and (EncLen = cn_mldsa_expected_pubkey_len(CN_MLDSA_TYPE_65)), 'cn_mldsa_generate_keys_65', Passed, Failed);
  Move('msg65'[1], InBlock[0], 5);
  R := cn_mldsa_sign(CN_MLDSA_TYPE_65, @RSABlock[0], RSALen, @InBlock[0], 5, nil, 0, CN_HASH_SHAKE128, nil, 0, @RSABlock[0], SizeOf(RSABlock), RSALen);
  Check((R = CN_OK) and (RSALen = cn_mldsa_expected_sig_len(CN_MLDSA_TYPE_65)), 'cn_mldsa_sign_65_shake128', Passed, Failed);
  R := cn_mldsa_verify(CN_MLDSA_TYPE_65, @Cipher[0], EncLen, @InBlock[0], 5, @RSABlock[0], RSALen, nil, 0, CN_HASH_SHAKE128);
  Check(R = 1, 'cn_mldsa_verify_65_shake128', Passed, Failed);

  R := cn_mldsa_generate_keys(CN_MLDSA_TYPE_87, nil, 0, @RSABlock[0], SizeOf(RSABlock), RSALen, @Cipher[0], SizeOf(Cipher), EncLen);
  Check((R = CN_OK) and (RSALen = cn_mldsa_expected_privkey_len(CN_MLDSA_TYPE_87)) and (EncLen = cn_mldsa_expected_pubkey_len(CN_MLDSA_TYPE_87)), 'cn_mldsa_generate_keys_87', Passed, Failed);
  Move('msg87'[1], InBlock[0], 5);
  R := cn_mldsa_sign(CN_MLDSA_TYPE_87, @RSABlock[0], RSALen, @InBlock[0], 5, nil, 0, CN_HASH_SHAKE256, nil, 0, @RSABlock[0], SizeOf(RSABlock), RSALen);
  Check((R = CN_OK) and (RSALen = cn_mldsa_expected_sig_len(CN_MLDSA_TYPE_87)), 'cn_mldsa_sign_87_shake256', Passed, Failed);
  R := cn_mldsa_verify(CN_MLDSA_TYPE_87, @Cipher[0], EncLen, @InBlock[0], 5, @RSABlock[0], RSALen, nil, 0, CN_HASH_SHAKE256);
  Check(R = 1, 'cn_mldsa_verify_87_shake256', Passed, Failed);

  R := cn_ecc_generate_keys(CN_ECC_CURVE_SECP256R1, Priv, Pub);
  Check(R = CN_OK, 'cn_ecc_generate_keys_secp256r1', Passed, Failed);
  Move('abc'[1], InBlock[0], 3);
  R := cn_ecc_sign(CN_HASH_SHA2_512, CN_ECC_CURVE_SECP256R1, Priv, @InBlock[0], 3, @OutBlock[0], SizeOf(OutBlock), OutLen);
  Check((R = CN_OK) and (OutLen > 0), 'cn_ecc_sign_secp256r1_sha512', Passed, Failed);
  R := cn_ecc_verify(CN_HASH_SHA2_512, CN_ECC_CURVE_SECP256R1, Pub, @InBlock[0], 3, @OutBlock[0], OutLen);
  Check(R = 1, 'cn_ecc_verify_secp256r1_sha512', Passed, Failed);
  R := cn_ecc_save_keys_to_pem(CN_ECC_KEY_PKCS8, CN_ECC_CURVE_SECP256R1, Priv, Pub, @Cipher[0], SizeOf(Cipher), EncLen);
  Check((R = CN_OK) and (EncLen > 0), 'cn_ecc_save_keys_to_pem_secp256r1_pkcs8', Passed, Failed);
  R := cn_ecc_key_free(Pub);
  Check(R = CN_OK, 'cn_ecc_key_free_pub_secp256r1', Passed, Failed);
  R := cn_ecc_key_free(Priv);
  Check(R = CN_OK, 'cn_ecc_key_free_priv_secp256r1', Passed, Failed);

  R := cn_ecc_generate_keys(CN_ECC_CURVE_SECP521R1, Priv, Pub);
  Check(R = CN_OK, 'cn_ecc_generate_keys_secp521r1', Passed, Failed);
  Move('xyz'[1], InBlock[0], 3);
  R := cn_ecc_sign(CN_HASH_SM3, CN_ECC_CURVE_SECP521R1, Priv, @InBlock[0], 3, @OutBlock[0], SizeOf(OutBlock), OutLen);
  Check((R = CN_OK) and (OutLen > 0), 'cn_ecc_sign_secp521r1_sm3', Passed, Failed);
  R := cn_ecc_verify(CN_HASH_SM3, CN_ECC_CURVE_SECP521R1, Pub, @InBlock[0], 3, @OutBlock[0], OutLen);
  Check(R = 1, 'cn_ecc_verify_secp521r1_sm3', Passed, Failed);
  R := cn_ecc_save_public_key_to_pem(CN_ECC_KEY_PKCS8, CN_ECC_CURVE_SECP521R1, Pub, @Cipher[0], SizeOf(Cipher), EncLen);
  Check((R = CN_OK) and (EncLen > 0), 'cn_ecc_save_public_key_to_pem_secp521r1_pkcs8', Passed, Failed);
  R := cn_ecc_key_free(Pub);
  Check(R = CN_OK, 'cn_ecc_key_free_pub_secp521r1', Passed, Failed);
  R := cn_ecc_key_free(Priv);
  Check(R = CN_OK, 'cn_ecc_key_free_priv_secp521r1', Passed, Failed);

  FillChar(Key[0], 16, 77);
  FillChar(Iv[0], 12, 88);
  FillChar(InBlock[0], 32, 99);
  Move('aad!'[1], Ctx[0], 4);
  R := cn_aead_encrypt(CN_AEAD_SM4_GCM, @Key[0], 16, @Iv[0], 12, @Ctx[0], 4, @InBlock[0], 32, @OutBlock[0], SizeOf(OutBlock), CipherLen, @Tag[0], SizeOf(Tag), TagLen);
  Check((R = CN_OK) and (CipherLen = 32) and (TagLen = CN_AEAD_TAG_BYTES), 'cn_aead_encrypt_sm4_gcm_aad', Passed, Failed);
  R := cn_aead_decrypt(CN_AEAD_SM4_GCM, @Key[0], 16, @Iv[0], 12, @Ctx[0], 4, @OutBlock[0], CipherLen, @Tag[0], TagLen, @PlainBack[0], SizeOf(PlainBack), OutLen);
  Check((R = CN_OK) and (OutLen = 32) and BytesEqual(@PlainBack[0], @InBlock[0], 32), 'cn_aead_decrypt_sm4_gcm_aad', Passed, Failed);

  FillChar(Key[0], 32, 66);
  FillChar(Iv[0], 24, 55);
  FillChar(InBlock[0], 64, 44);
  Move('ctx+'[1], Ctx[0], 4);
  R := cn_aead_encrypt(CN_AEAD_XCHACHA20_POLY1305, @Key[0], 32, @Iv[0], 24, @Ctx[0], 4, @InBlock[0], 64, @OutBlock[0], SizeOf(OutBlock), CipherLen, @Tag[0], SizeOf(Tag), TagLen);
  Check((R = CN_OK) and (CipherLen = 64) and (TagLen = CN_AEAD_TAG_BYTES), 'cn_aead_encrypt_xchacha20_poly1305_aad', Passed, Failed);
  R := cn_aead_decrypt(CN_AEAD_XCHACHA20_POLY1305, @Key[0], 32, @Iv[0], 24, @Ctx[0], 4, @OutBlock[0], CipherLen, @Tag[0], TagLen, @PlainBack[0], SizeOf(PlainBack), OutLen);
  Check((R = CN_OK) and (OutLen = 64) and BytesEqual(@PlainBack[0], @InBlock[0], 64), 'cn_aead_decrypt_xchacha20_poly1305_aad', Passed, Failed);

  R := cn_rsa_generate_keys(1024, 1, Priv, Pub, 0);
  Check(R = CN_OK, 'cn_rsa_generate_keys_pkcs8', Passed, Failed);
  R := cn_rsa_save_keys_to_pem(CN_RSA_KEY_PKCS8, Priv, Pub, @Cipher[0], SizeOf(Cipher), EncLen);
  Check((R = CN_OK) and (EncLen > 0), 'cn_rsa_save_keys_to_pem_pkcs8', Passed, Failed);
  R := cn_rsa_load_keys_from_pem(@Cipher[0], EncLen, nil, 0, Priv, Pub);
  Check(R = CN_OK, 'cn_rsa_load_keys_from_pem_pkcs8', Passed, Failed);
  R := cn_rsa_save_public_key_to_pem(CN_RSA_KEY_PKCS8, Pub, @Cipher[0], SizeOf(Cipher), EncLen);
  Check((R = CN_OK) and (EncLen > 0), 'cn_rsa_save_public_key_to_pem_pkcs8', Passed, Failed);
  if R = CN_OK then
  begin
    Move('xyz'[1], InBlock[0], 3);
    R := cn_rsa_sign(CN_HASH_SHA2_512, Priv, @InBlock[0], 3, @RSABlock[0], SizeOf(RSABlock), RSALen);
    Check((R = CN_OK) and (RSALen > 0), 'cn_rsa_sign_pkcs8_sha512', Passed, Failed);
    R := cn_rsa_verify(CN_HASH_SHA2_512, Pub, @InBlock[0], 3, @RSABlock[0], RSALen);
    Check(R = 1, 'cn_rsa_verify_pkcs8_sha512', Passed, Failed);
    R := cn_rsa_key_free(Pub);
    Check(R = CN_OK, 'cn_rsa_key_free_pub_pkcs8', Passed, Failed);
    R := cn_rsa_key_free(Priv);
    Check(R = CN_OK, 'cn_rsa_key_free_priv_pkcs8', Passed, Failed);
  end;

  Print('TOTAL ' + IntToStr(Passed) + ' passed, ' + IntToStr(Failed) + ' failed');
end;

end.
