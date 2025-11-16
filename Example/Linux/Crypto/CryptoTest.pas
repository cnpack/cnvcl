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

unit CryptoTest;
{* |<PRE>
================================================================================
* 软件名称：CnPack 密码库
* 单元名称：CnPack 密码库批量测试单元
* 单元作者：CnPack 开发组 (master@cnpack.org)
* 备    注：测试失败的用例会通过 Assert 抛出异常
* 开发平台：PWin7 + Delphi 5.0
* 兼容测试：编译器：Delphi 5~2007 的非 Unicode、Delphi 2009 或以上的 Unicode、FPC 3.2 以上
*           CPU：Intel 32 位、Intel 64 位、ARM 32/64 位、龙芯 64 位
*           OS: Win32、Win64、MacOS64、Linux64
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2023.03.10 V1.0
*               创建单元，持续增加功能
================================================================================
|</PRE>}

interface

// 注意为了保持测试用例的纯净性，不能 {$I CnPack.inc}

{$IFDEF FPC}
// FPC 下要关闭 Range Check 以避免编译出错
{$R-}
{$ENDIF}

uses
  SysUtils, Classes, {$IFDEF ANDROID} FMX.Types, {$ENDIF}
  CnNative, CnBigNumber, CnSM4, CnDES, CnAES, CnAEAD, CnRSA, CnECC, CnSM2, CnSM3,
  CnSM9, CnFNV, CnKDF, CnBase64, CnCRC32, CnMD5, CnSHA1, CnSHA2, CnSHA3, CnChaCha20,
  CnPoly1305, CnTEA, CnZUC, CnFEC, CnPrime, Cn25519, CnPaillier, CnSecretSharing,
  CnPolynomial, CnBits, CnLattice, CnOTS, CnPemUtils, CnInt128, CnRC4, CnPDFCrypt,
  CnDSA, CnBLAKE, CnBLAKE2, CnXXH, CnWideStrings, CnContainers, CnMLKEM;

procedure TestCrypto;
{* 密码库总测试入口}

// ============================== Native =======================================

function TestEndian: Boolean;
function TestStrToUInt64: Boolean;
function TestUInt64Div: Boolean;
function TestUInt64Mod: Boolean;

// =========================== Constant Time ===================================

function TestConstTimeSwap: Boolean;
function TestConstTimeSelect: Boolean;
function TestConstTimeEqual: Boolean;
function TestConstTimeExpandBool: Boolean;
function TestConstTimeBytes: Boolean;

// ============================== Strings ======================================

function TestUtf8: Boolean;

// ============================== BigNumber ====================================

function TestBigNumberHex: Boolean;
function TestBigNumberDec: Boolean;
function TestBigNumberExpandWord: Boolean;
function TestBigNumberMulWord: Boolean;
function TestBigNumberModWord: Boolean;
function TestBigNumberDivWord: Boolean;
function TestBigNumberUnsignedAdd: Boolean;
function TestBigNumberPowerMod: Boolean;
function TestBigNumberDiv: Boolean;
function TestBigNumberRoundDiv: Boolean;
function TestBigNumberShiftLeft: Boolean;
function TestBigNumberGetBitsCount: Boolean;
function TestBigNumberShiftRightOne: Boolean;
function TestBigNumberGcd: Boolean;
function TestBigNumberLcm: Boolean;
function TestBigNumberFermatCheckComposite: Boolean;
function TestBigNumberIsProbablyPrime: Boolean;
function TestBigNumberIsPerfectPower: Boolean;
function TestBigNumberJacobiSymbol: Boolean;
function TestBigNumberMersennePrime: Boolean;
function TestBigNumberAKSIsPrime: Boolean;
function TestBigNumberBPSWIsPrime: Boolean;

// ================================ Bits =======================================

function TestBitsEmpty: Boolean;
function TestBitsAppend: Boolean;
function TestBitsDelete: Boolean;

// =============================== Int128 ======================================

function TestInt128Add: Boolean;
function TestInt128Sub: Boolean;
function TestInt128Mul: Boolean;
function TestInt128DivMod: Boolean;
function TestUInt128Add: Boolean;
function TestUInt128Sub: Boolean;
function TestUInt128Mul: Boolean;
function TestUInt128DivMod: Boolean;

// ============================= Polynomial ====================================

function TestBigNumberPolynomialGaloisPrimePowerModularInverse: Boolean;

// ============================== Lattice ======================================

function TestNTRUHPS2048509: Boolean;
function TestNTRUHPS2048677: Boolean;
function TestNTRUHPS4096821: Boolean;
function TestMLKEM512KeyGen: Boolean;
function TestMLKEM512KeyEncapDecap: Boolean;
function TestMLKEM768KeyGen: Boolean;
function TestMLKEM768KeyEncapDecap: Boolean;
function TestMLKEM1024KeyGen: Boolean;
function TestMLKEM1024KeyEncapDecap: Boolean;

// ================================ SM4 ========================================

function TestSM4Standard1: Boolean;
function TestSM4Standard2: Boolean;
function TestSM4Ecb: Boolean;
function TestSM4Cbc: Boolean;
function TestSM4Cfb: Boolean;
function TestSM4Ofb: Boolean;
function TestSM4Ctr: Boolean;

// ================================ DES ========================================

function TestDESEcb: Boolean;
function TestDESCbc: Boolean;

// ================================ 3DES =======================================

function Test3DESEcb: Boolean;
function Test3DESCbc: Boolean;

// ================================ AES ========================================

function TestAESEcb128: Boolean;
function TestAESEcb192: Boolean;
function TestAESEcb256: Boolean;
function TestAESCbc128: Boolean;
function TestAESCbc192: Boolean;
function TestAESCbc256: Boolean;
function TestAESCfb128: Boolean;
function TestAESCfb192: Boolean;
function TestAESCfb256: Boolean;
function TestAESOfb128: Boolean;
function TestAESOfb192: Boolean;
function TestAESOfb256: Boolean;
function TestAESCtr128: Boolean;
function TestAESCtr192: Boolean;
function TestAESCtr256: Boolean;

// ================================ CRC ========================================

function TestCRC8CCITT: Boolean;
function TestCRC16CCITT: Boolean;
function TestCRC32: Boolean;
function TestCRC64ECMA: Boolean;

// ================================ XXH ========================================

function TestXXH32: Boolean;
function TestXXH64: Boolean;

// ================================ MD5 ========================================

function TestMD5: Boolean;
function TestMD5Hmac: Boolean;
function TestMD5Update: Boolean;

// ================================ SHA1 =======================================

function TestSHA1: Boolean;
function TestSHA1HMac: Boolean;
function TestSHA1Update: Boolean;

// ================================ SHA2 =======================================

function TestSHA224: Boolean;
function TestSHA224HMac: Boolean;
function TestSHA224Update: Boolean;
function TestSHA256: Boolean;
function TestSHA256HMac: Boolean;
function TestSHA256Update: Boolean;
function TestSHA384: Boolean;
function TestSHA384HMac: Boolean;
function TestSHA384Update: Boolean;
function TestSHA512: Boolean;
function TestSHA512HMac: Boolean;
function TestSHA512Update: Boolean;

// ================================ SHA3 =======================================

function TestSHA3_224: Boolean;
function TestSHA3_224HMac: Boolean;
function TestSHA3_224Update: Boolean;
function TestSHA3_256: Boolean;
function TestSHA3_256HMac: Boolean;
function TestSHA3_256Update: Boolean;
function TestSHA3_384: Boolean;
function TestSHA3_384HMac: Boolean;
function TestSHA3_384Update: Boolean;
function TestSHA3_512: Boolean;
function TestSHA3_512HMac: Boolean;
function TestSHA3_512Update: Boolean;
function TestSHAKE1281: Boolean;
function TestSHAKE1282: Boolean;
function TestSHAKE2561: Boolean;
function TestSHAKE2562: Boolean;

// ================================ BLAKE ======================================

function TestBLAKE224: Boolean;
function TestBLAKE224HMac: Boolean;
function TestBLAKE224Update: Boolean;
function TestBLAKE256: Boolean;
function TestBLAKE256HMac: Boolean;
function TestBLAKE256Update: Boolean;
function TestBLAKE384: Boolean;
function TestBLAKE384HMac: Boolean;
function TestBLAKE384Update: Boolean;
function TestBLAKE512: Boolean;
function TestBLAKE512HMac: Boolean;
function TestBLAKE512Update: Boolean;

// =============================== BLAKE2 ======================================

function TestBLAKE2S: Boolean;
function TestBLAKE2B: Boolean;
function TestBLAKE2SKey: Boolean;
function TestBLAKE2BKey: Boolean;
function TestBLAKE2SUpdate: Boolean;
function TestBLAKE2BUpdate: Boolean;

// =============================== Base64 ======================================

function TestBase64: Boolean;
function TestBase64URL: Boolean;

// ================================ AEAD =======================================

function TestAEADAESCCM: Boolean;
function TestAEADSM4CCM: Boolean;
function TestAEADAES128GCM: Boolean;
function TestAEADAES192GCM: Boolean;
function TestAEADAES256GCM: Boolean;
function TestAEADSM4GCM: Boolean;
function TestAEADChaCha20Poly1305: Boolean;
function TestAEADXChaCha20Poly1305: Boolean;

// ================================ ChaCha20 ===================================

function TestChaCha20: Boolean;
function TestHChaCha20SubKey: Boolean;
function TestXChaCha20: Boolean;

// ================================ Poly1305 ===================================

function TestPoly1305: Boolean;
function TestPoly1305Update: Boolean;

// ================================ ZUC ========================================

function TestZUC1: Boolean;
function TestZUC2: Boolean;
function TestZUC3: Boolean;
function TestZUC4: Boolean;
function TestZUCEEA31: Boolean;
function TestZUCEEA32: Boolean;
function TestZUCEEA33: Boolean;
function TestZUCEIA31: Boolean;
function TestZUCEIA32: Boolean;
function TestZUCEIA33: Boolean;

// ================================ RC4 ========================================

function TestRC4: Boolean;

// ================================ TEA ========================================

function TestTea: Boolean;
function TestXTea: Boolean;
function TestXXTea: Boolean;

// ================================ FNV ========================================

function TestFNV1: Boolean;
function TestFNV1a: Boolean;

// ================================ FEC ========================================

function TestHamming: Boolean;

// ================================ DSA ========================================

function TestDSA1: Boolean;
function TestDSA2: Boolean;
function TestDSA3: Boolean;

// ================================ PDF ========================================

function TestPDFCalcOwnerPassword: Boolean;
function TestPDFCalcUserPassword: Boolean;
function TestPDFCheckOwnerPassword: Boolean;

// ================================ SM2 ========================================

function TestSM21: Boolean;
function TestSM22: Boolean;
function TestSM23: Boolean;

// ================================ SM3 ========================================

function TestSM3: Boolean;
function TestSM3HMac: Boolean;
function TestSM3Update: Boolean;

// ================================ SM9 ========================================

function TestSM9Hash1: Boolean;
function TestSM9Hash2: Boolean;
function TestSM9Mac: Boolean;
function TestSM9Sign: Boolean;
function TestSM9KeyExchange: Boolean;
function TestSM9KeyEncapsulation: Boolean;
function TestSM9PublicEncryption: Boolean;

// ================================ RSA ========================================

function TestRSA1: Boolean;
function TestRSA2: Boolean;
function TestRSA3: Boolean;
function TestRSAPrivPubPkcs1: Boolean;
function TestRSAPubPkcs1: Boolean;
function TestRSAPrivPubPkcs8: Boolean;
function TestRSAPubPkcs8: Boolean;
function TestChameleonHash: Boolean;
function TestRSA2Crypt: Boolean;
function TestRSALongStream1: Boolean;
function TestRSALongStream2: Boolean;

// ================================ KDF ========================================

function TestKDFPB1: Boolean;
function TestKDFPB2: Boolean;
function TestKDFSM2SM9: Boolean;
function TestHKDF: Boolean;

// ================================ Prime Number ===============================

function TestPrimeNumber1: Boolean;
function TestPrimeNumber2: Boolean;
function TestPrimeNumber3: Boolean;
function TestPrimeNumber4: Boolean;
function TestPrimeNumber5: Boolean;
function TestSquareRoot: Boolean;
function TestBPSWIsPrime: Boolean;

// ================================ 25519 ======================================

function Test25519CurveMul: Boolean;
function Test25519CurveGMul: Boolean;
function Test25519KeyExchange: Boolean;
function Test25519CalcKey: Boolean;
function Test25519Sign: Boolean;
function Test448CurveMul: Boolean;
function Test448CurveGMul: Boolean;
function Test448KeyExchange: Boolean;
function Test448CalcKey: Boolean;
function Test448Sign1: Boolean;
function Test448Sign2: Boolean;

// =============================== Paillier ====================================

function TestPaillier1: Boolean;
function TestPaillier2: Boolean;

// ============================= SecretSharing =================================

function TestSecretSharingShamir: Boolean;
function TestSecretSharingFeldmanVss: Boolean;

// ================================ OTS ========================================

function TestOTSSM3: Boolean;
function TestOTSSHA256: Boolean;
function TestMOTSSM3: Boolean;
function TestMOTSSHA256: Boolean;
function TestWOTSSM3: Boolean;
function TestWOTSSHA256: Boolean;

// ================================ ECC ========================================

function TestECCMul: Boolean;
function TestECCPrivPubPkcs1: Boolean;
function TestECCPrivPubPkcs8: Boolean;
function TestECCPub: Boolean;
function TestECCSchoof: Boolean;
function TestECCSchoof2: Boolean;

// ================================= END =======================================

implementation

const
  SCRLF = #13#10;

procedure MyWriteln(const Text: string);
begin
{$IFDEF ANDROID}
  Log.D(Text);
{$ELSE}
  Writeln(Text);
{$ENDIF}
end;

procedure MyAssert(V: Boolean; const Msg: string);
begin
  MyWriteln(FormatDateTime('yyyy-MM-dd:hh:nn:ss.zzz | ', Now) +  Msg + '...');
  Assert(V);
end;

procedure TestCrypto;
begin
{$IFDEF CPU64BITS}
  MyWriteln('*** CPU 64 Bits ***');
{$ELSE}
  MyWriteln('*** CPU 32 Bits ***');
{$ENDIF}

{$IFDEF CPUARM}
  MyWriteln('*** ARM ***');
{$ENDIF}

  if CurrentByteOrderIsBigEndian then
    MyWriteln('=== Big Endian ===');
  if CurrentByteOrderIsLittleEndian then
    MyWriteln('=== Little Endian ===');

  MyWriteln('Crypto Test Start...');

// ============================== Native =======================================

  MyAssert(TestEndian, 'TestEndian');
  MyAssert(TestStrToUInt64, 'TestStrToUInt64');
  MyAssert(TestUInt64Div, 'TestUInt64Div');
  MyAssert(TestUInt64Mod, 'TestUInt64Mod');

// =========================== Constant Time ===================================

  MyAssert(TestConstTimeSwap, 'TestConstTimeSwap');
  MyAssert(TestConstTimeSelect, 'TestConstTimeSelect');
  MyAssert(TestConstTimeEqual, 'TestConstTimeEqual');
  MyAssert(TestConstTimeExpandBool, 'TestConstTimeExpandBool');
  MyAssert(TestConstTimeBytes, 'TestConstTimeBytes');

// ============================== Strings ======================================

  MyAssert(TestUtf8, 'TestUtf8');

// ============================== BigNumber ====================================

  MyAssert(TestBigNumberHex, 'TestBigNumberHex');
  MyAssert(TestBigNumberDec, 'TestBigNumberDec');
  MyAssert(TestBigNumberExpandWord, 'TestBigNumberExpandWord');
  MyAssert(TestBigNumberModWord, 'TestBigNumberModWord');
  MyAssert(TestBigNumberMulWord, 'TestBigNumberMulWord');
  MyAssert(TestBigNumberDivWord, 'TestBigNumberDivWord');
  MyAssert(TestBigNumberUnsignedAdd, 'TestBigNumberUnsignedAdd');
  MyAssert(TestBigNumberPowerMod, 'TestBigNumberPowerMod');
  MyAssert(TestBigNumberDiv, 'TestBigNumberDiv');
  MyAssert(TestBigNumberRoundDiv, 'TestBigNumberRoundDiv');
  MyAssert(TestBigNumberShiftLeft, 'TestBigNumberShiftLeft');
  MyAssert(TestBigNumberGetBitsCount, 'TestBigNumberGetBitsCount');
  MyAssert(TestBigNumberShiftRightOne, 'TestBigNumberShiftRightOne');
  MyAssert(TestBigNumberGcd, 'TestBigNumberGcd');
  MyAssert(TestBigNumberLcm, 'TestBigNumberLcm');
  MyAssert(TestBigNumberFermatCheckComposite, 'TestBigNumberFermatCheckComposite');
  MyAssert(TestBigNumberIsProbablyPrime, 'TestBigNumberIsProbablyPrime');
  MyAssert(TestBigNumberIsPerfectPower, 'TestBigNumberIsPerfectPower');
  MyAssert(TestBigNumberJacobiSymbol, 'TestBigNumberJacobiSymbol');
  MyAssert(TestBigNumberMersennePrime, 'TestBigNumberMersennePrime');
  MyAssert(TestBigNumberAKSIsPrime, 'TestBigNumberAKSIsPrime');
  MyAssert(TestBigNumberBPSWIsPrime, 'TestBigNumberBPSWIsPrime');

// ================================ Bits =======================================

  MyAssert(TestBitsEmpty, 'TestBitsEmpty');
  MyAssert(TestBitsAppend, 'TestBitsAppend');
  MyAssert(TestBitsDelete, 'TestBitsDelete');

// =============================== Int128 ======================================

  MyAssert(TestInt128Add, 'TestInt128Add');
  MyAssert(TestInt128Sub, 'TestInt128Sub');
  MyAssert(TestInt128Mul, 'TestInt128Mul');
  MyAssert(TestInt128DivMod, 'TestInt128DivMod');
  MyAssert(TestUInt128Add, 'TestUInt128Add');
  MyAssert(TestUInt128Sub, 'TestUInt128Sub');
  MyAssert(TestUInt128Mul, 'TestUInt128Mul');
  MyAssert(TestUInt128DivMod, 'TestUInt128DivMod');

// ============================= Polynomial ====================================

  MyAssert(TestBigNumberPolynomialGaloisPrimePowerModularInverse, 'TestBigNumberPolynomialGaloisPrimePowerModularInverse');

// ============================== Lattice ======================================

  MyAssert(TestNTRUHPS2048509, 'TestNTRUHPS2048509');
  MyAssert(TestNTRUHPS2048677, 'TestNTRUHPS2048677');
  MyAssert(TestNTRUHPS4096821, 'TestNTRUHPS4096821');
  MyAssert(TestMLKEM512KeyGen, 'TestMLKEM512KeyGen');
  MyAssert(TestMLKEM512KeyEncapDecap, 'TestMLKEM512KeyEncapDecap');
  MyAssert(TestMLKEM768KeyGen, 'TestMLKEM768KeyGen');
  MyAssert(TestMLKEM768KeyEncapDecap, 'TestMLKEM768KeyEncapDecap');
  MyAssert(TestMLKEM1024KeyGen, 'TestMLKEM1024KeyGen');
  MyAssert(TestMLKEM1024KeyEncapDecap, 'TestMLKEM1024KeyEncapDecap');

// ================================ SM4 ========================================

  MyAssert(TestSM4Standard1, 'TestSM4Standard1');
  MyAssert(TestSM4Standard2, 'TestSM4Standard2');
  MyAssert(TestSM4Ecb, 'TestSM4Ecb');
  MyAssert(TestSM4Cbc, 'TestSM4Cbc');
  MyAssert(TestSM4Cfb, 'TestSM4Cfb');
  MyAssert(TestSM4Ofb, 'TestSM4Ofb');
  MyAssert(TestSM4Ctr, 'TestSM4Ctr');

// ================================ DES ========================================

  MyAssert(TestDESEcb, 'TestDESEcb');
  MyAssert(TestDESCbc, 'TestDESCbc');

// ================================ 3DES =======================================

  MyAssert(Test3DESEcb, 'Test3DESEcb');
  MyAssert(Test3DESCbc, 'Test3DESCbc');

// ================================ AES ========================================

  MyAssert(TestAESEcb128, 'TestAESEcb128');
  MyAssert(TestAESEcb192, 'TestAESEcb192');
  MyAssert(TestAESEcb256, 'TestAESEcb256');
  MyAssert(TestAESCbc128, 'TestAESCbc128');
  MyAssert(TestAESCbc192, 'TestAESCbc192');
  MyAssert(TestAESCbc256, 'TestAESCbc256');
  MyAssert(TestAESCfb128, 'TestAESCfb128');
  MyAssert(TestAESCfb192, 'TestAESCfb192');
  MyAssert(TestAESCfb256, 'TestAESCfb256');
  MyAssert(TestAESOfb128, 'TestAESOfb128');
  MyAssert(TestAESOfb192, 'TestAESOfb192');
  MyAssert(TestAESOfb256, 'TestAESOfb256');
  MyAssert(TestAESCtr128, 'TestAESCtr128');
  MyAssert(TestAESCtr192, 'TestAESCtr192');
  MyAssert(TestAESCtr256, 'TestAESCtr256');

// ================================ CRC ========================================

  MyAssert(TestCRC8CCITT, 'TestCRC8CCITT');
  MyAssert(TestCRC16CCITT, 'TestCRC16CCITT');
  MyAssert(TestCRC32, 'TestCRC32');
  MyAssert(TestCRC64ECMA, 'TestCRC64ECMA');

// ============================== XXHash =======================================

  MyAssert(TestXXH32, 'TestXXH32');
  MyAssert(TestXXH64, 'TestXXH64');

// ================================ MD5 ========================================

  MyAssert(TestMD5, 'TestMD5');
  MyAssert(TestMD5Hmac, 'TestMD5Hmac');
  MyAssert(TestMD5Update, 'TestMD5Update');

// ================================ SHA1 =======================================

  MyAssert(TestSHA1, 'TestSHA1');
  MyAssert(TestSHA1Hmac, 'TestSHA1Hmac');
  MyAssert(TestSHA1Update, 'TestSHA1Update');

// ================================ SHA2 =======================================

  MyAssert(TestSHA224, 'TestSHA224');
  MyAssert(TestSHA224HMac, 'TestSHA224HMac');
  MyAssert(TestSHA224Update, 'TestSHA224Update');
  MyAssert(TestSHA256, 'TestSHA256');
  MyAssert(TestSHA256HMac, 'TestSHA256HMac');
  MyAssert(TestSHA256Update, 'TestSHA256Update');
  MyAssert(TestSHA384, 'TestSHA384');
  MyAssert(TestSHA384HMac, 'TestSHA384HMac');
  MyAssert(TestSHA384Update, 'TestSHA384Update');
  MyAssert(TestSHA512, 'TestSHA512');
  MyAssert(TestSHA512HMac, 'TestSHA512HMac');
  MyAssert(TestSHA512Update, 'TestSHA512Update');

// ================================ SHA3 =======================================

  MyAssert(TestSHA3_224, 'TestSHA3_224');
  MyAssert(TestSHA3_224HMac, 'TestSHA3_224HMac');
  MyAssert(TestSHA3_224Update, 'TestSHA3_224Update');
  MyAssert(TestSHA3_256, 'TestSHA3_256');
  MyAssert(TestSHA3_256HMac, 'TestSHA3_256HMac');
  MyAssert(TestSHA3_256Update, 'TestSHA3_256Update');
  MyAssert(TestSHA3_384, 'TestSHA3_384');
  MyAssert(TestSHA3_384HMac, 'TestSHA3_384HMac');
  MyAssert(TestSHA3_384Update, 'TestSHA3_384Update');
  MyAssert(TestSHA3_512, 'TestSHA3_512');
  MyAssert(TestSHA3_512HMac, 'TestSHA3_512HMac');
  MyAssert(TestSHA3_512Update, 'TestSHA3_512Update');
  MyAssert(TestSHAKE1281, 'TestSHAKE1281');
  MyAssert(TestSHAKE1282, 'TestSHAKE1282');
  MyAssert(TestSHAKE2561, 'TestSHAKE2561');
  MyAssert(TestSHAKE2562, 'TestSHAKE2562');

// ================================ BLAKE ======================================

  MyAssert(TestBLAKE224, 'TestBLAKE224');
  MyAssert(TestBLAKE224HMac, 'TestBLAKE224HMac');
  MyAssert(TestBLAKE224Update, 'TestBLAKE224Update');
  MyAssert(TestBLAKE256, 'TestBLAKE256');
  MyAssert(TestBLAKE256HMac, 'TestBLAKE256HMac');
  MyAssert(TestBLAKE256Update, 'TestBLAKE256Update');
  MyAssert(TestBLAKE384, 'TestBLAKE384');
  MyAssert(TestBLAKE384HMac, 'TestBLAKE384HMac');
  MyAssert(TestBLAKE384Update, 'TestBLAKE384Update');
  MyAssert(TestBLAKE512, 'TestBLAKE512');
  MyAssert(TestBLAKE512HMac, 'TestBLAKE512HMac');
  MyAssert(TestBLAKE512Update, 'TestBLAKE512Update');

// =============================== BLAKE2 ======================================

  MyAssert(TestBLAKE2S, 'TestBLAKE2S');
  MyAssert(TestBLAKE2B, 'TestBLAKE2B');
  MyAssert(TestBLAKE2SKey, 'TestBLAKE2SKey');
  MyAssert(TestBLAKE2BKey, 'TestBLAKE2BKey');
  MyAssert(TestBLAKE2SUpdate, 'TestBLAKE2SUpdate');
  MyAssert(TestBLAKE2BUpdate, 'TestBLAKE2BUpdate');

// ================================ Base64 =====================================

  MyAssert(TestBase64, 'TestBase64');
  MyAssert(TestBase64URL, 'TestBase64URL');

// ================================ AEAD =======================================

  MyAssert(TestAEADAESCCM, 'TestAEADAESCCM');
  MyAssert(TestAEADSM4CCM, 'TestAEADSM4CCM');
  MyAssert(TestAEADAES128GCM, 'TestAEADAES128GCM');
  MyAssert(TestAEADAES192GCM, 'TestAEADAES192GCM');
  MyAssert(TestAEADAES256GCM, 'TestAEADAES256GCM');
  MyAssert(TestAEADSM4GCM, 'TestAEADSM4GCM');
  MyAssert(TestAEADChaCha20Poly1305, 'TestAEADChaCha20Poly1305');
  MyAssert(TestAEADXChaCha20Poly1305, 'TestAEADXChaCha20Poly1305');

// ================================ ChaCha20 ===================================

  MyAssert(TestChaCha20, 'TestChaCha20');
  MyAssert(TestHChaCha20SubKey, 'TestHChaCha20SubKey');
  MyAssert(TestXChaCha20, 'TestXChaCha20');

// ================================ Poly1305 ===================================

  MyAssert(TestPoly1305, 'TestPoly1305');

// ================================ ZUC ========================================

  MyAssert(TestZUC1, 'TestZUC1');
  MyAssert(TestZUC2, 'TestZUC2');
  MyAssert(TestZUC3, 'TestZUC3');
  MyAssert(TestZUC4, 'TestZUC4');
  MyAssert(TestZUCEEA31, 'TestZUCEEA31');
  MyAssert(TestZUCEEA32, 'TestZUCEEA32');
  MyAssert(TestZUCEEA33, 'TestZUCEEA33');
  MyAssert(TestZUCEIA31, 'TestZUCEIA31');
  MyAssert(TestZUCEIA32, 'TestZUCEIA32');
  MyAssert(TestZUCEIA33, 'TestZUCEIA33');

// ================================ ZUC ========================================

  MyAssert(TestRC4, 'TestRC4');

// ================================ TEA ========================================

  MyAssert(TestTea, 'TestTea');
  MyAssert(TestXTea, 'TestXTea');
  MyAssert(TestXXTea, 'TestXXTea');

// ================================ FNV ========================================

  MyAssert(TestFNV1, 'TestFNV1');
  MyAssert(TestFNV1a, 'TestFNV1a');

// ================================ FEC ========================================

  MyAssert(TestHamming, 'TestHamming');

// ================================ DSA ========================================

  MyAssert(TestDSA1, 'TestDSA1');
  MyAssert(TestDSA2, 'TestDSA2');
  MyAssert(TestDSA3, 'TestDSA3');

// ================================ PDF ========================================

  MyAssert(TestPDFCalcOwnerPassword, 'TestPDFCalcOwnerPassword');
  MyAssert(TestPDFCalcUserPassword, 'TestPDFCalcUserPassword');
  MyAssert(TestPDFCheckOwnerPassword, 'TestPDFCheckOwnerPassword');

// ================================ SM2 ========================================

  MyAssert(TestSM21, 'TestSM21');
  MyAssert(TestSM22, 'TestSM22');
  MyAssert(TestSM23, 'TestSM23');

// ================================ SM3 ========================================

  MyAssert(TestSM3, 'TestSM3');
  MyAssert(TestSM3Hmac, 'TestSM3Hmac');
  MyAssert(TestSM3Update, 'TestSM3Update');

// ================================ SM9 ========================================

  MyAssert(TestSM9Hash1, 'TestSM9Hash1');
  MyAssert(TestSM9Hash2, 'TestSM9Hash2');
  MyAssert(TestSM9Mac, 'TestSM9Mac');

  MyAssert(TestSM9Sign, 'TestSM9Sign');
  MyAssert(TestSM9KeyExchange, 'TestSM9KeyExchange');
  MyAssert(TestSM9KeyEncapsulation, 'TestSM9KeyEncapsulation');
  MyAssert(TestSM9PublicEncryption, 'TestSM9PublicEncryption');

// ================================ RSA ========================================

  MyAssert(TestRSA1, 'TestRSA1');
  MyAssert(TestRSA2, 'TestRSA2');
  MyAssert(TestRSA3, 'TestRSA3');
  MyAssert(TestRSAPrivPubPkcs1, 'TestRSAPrivPubPkcs1');
  MyAssert(TestRSAPubPkcs1, 'TestRSAPubPkcs1');
  MyAssert(TestRSAPrivPubPkcs8, 'TestRSAPrivPubPkcs8');
  MyAssert(TestRSAPubPkcs8, 'TestRSAPubPkcs8');
  MyAssert(TestChameleonHash, 'TestChameleonHash');
  MyAssert(TestRSA2Crypt, 'TestRSA2Crypt');
  MyAssert(TestRSALongStream1, 'TestRSALongStream1');
  MyAssert(TestRSALongStream2, 'TestRSALongStream2');

// ================================ KDF ========================================

  MyAssert(TestKDFPB1, 'TestKDFPB1');
  MyAssert(TestKDFPB2, 'TestKDFPB2');
  MyAssert(TestKDFSM2SM9, 'TestKDFSM2SM9');
  MyAssert(TestHKDF, 'TestHKDF');

// ================================ Prime Number ===============================

  MyAssert(TestPrimeNumber1, 'TestPrimeNumber1');
  MyAssert(TestPrimeNumber2, 'TestPrimeNumber2');
  MyAssert(TestPrimeNumber3, 'TestPrimeNumber3');
  MyAssert(TestPrimeNumber4, 'TestPrimeNumber4');
  MyAssert(TestPrimeNumber5, 'TestPrimeNumber5');
  MyAssert(TestSquareRoot, 'TestSquareRoot');
  MyAssert(TestBPSWIsPrime, 'TestBPSWIsPrime');

// ================================ 25519 ======================================

  MyAssert(Test25519CurveMul, 'Test25519CurveMul');
  MyAssert(Test25519CurveGMul, 'Test25519CurveGMul');
  MyAssert(Test25519KeyExchange, 'Test25519KeyExchange');
  MyAssert(Test25519CalcKey, 'Test25519CalcKey');
  MyAssert(Test25519Sign, 'Test25519Sign');
  MyAssert(Test448CurveMul, 'Test448CurveMul');
  MyAssert(Test448CurveGMul, 'Test448CurveGMul');
  MyAssert(Test448KeyExchange, 'Test448KeyExchange');
  MyAssert(Test448CalcKey, 'Test448CalcKey');
  MyAssert(Test448Sign1, 'Test448Sign1');
  MyAssert(Test448Sign2, 'Test448Sign2');

// =============================== Paillier ====================================

  MyAssert(TestPaillier1, 'TestPaillier1');
  MyAssert(TestPaillier2, 'TestPaillier2');

// ============================= SecretSharing =================================

  MyAssert(TestSecretSharingShamir, 'TestSecretSharingShamir');
  MyAssert(TestSecretSharingFeldmanVss, 'TestSecretSharingFeldmanVss');

// ================================ OTS ========================================

  MyAssert(TestOTSSM3, 'TestOTSSM3');
  MyAssert(TestOTSSHA256, 'TestOTSSHA256');
  MyAssert(TestMOTSSM3, 'TestMOTSSM3');
  MyAssert(TestMOTSSHA256, 'TestMOTSSHA256');
  MyAssert(TestWOTSSM3, 'TestWOTSSM3');
  MyAssert(TestWOTSSHA256, 'TestWOTSSHA256');

// ================================ ECC ========================================

  MyAssert(TestECCMul, 'TestECCMul');
  MyAssert(TestECCPrivPubPkcs1, 'TestECCPrivPubPkcs1');
  MyAssert(TestECCPrivPubPkcs8, 'TestECCPrivPubPkcs8');
  MyAssert(TestECCPub, 'TestECCPub');
  MyAssert(TestECCSchoof, 'TestECCSchoof');
  MyAssert(TestECCSchoof2, 'TestECCSchoof2');

// ================================= END =======================================

  MyWriteln('Crypto Test End.');
end;

// ============================== Native =======================================

function TestEndian: Boolean;
var
  A16, B16, C16: Word;
  A32, B32, C32: Cardinal;
  A64, B64, C64: TUInt64;
begin
  A16 := $D280;
  B16 := UInt16ToBigEndian(A16);
  C16 := UInt16ToLittleEndian(A16);
  Result := (DataToHex(@B16, SizeOf(B16)) = 'D280')  and (DataToHex(@C16, SizeOf(C16)) = '80D2');

  if not Result then Exit;

  A32 := $1D327806;
  B32 := UInt32ToBigEndian(A32);
  C32 := UInt32ToLittleEndian(A32);
  Result := (DataToHex(@B32, SizeOf(B32)) = '1D327806')  and (DataToHex(@C32, SizeOf(C32)) = '0678321D');

  if not Result then Exit;

  A64 := $2A64C05397B3C10D;;
  B64 := UInt64ToBigEndian(A64);
  C64 := UInt64ToLittleEndian(A64);
  Result := (DataToHex(@B64, SizeOf(B64)) = '2A64C05397B3C10D')  and (DataToHex(@C64, SizeOf(C64)) = '0DC1B39753C0642A');
end;

function TestStrToUInt64: Boolean;
var
  A: TUInt64;
  S: string;
begin
  S := '10977225559701242671';
  A := StrToUInt64(S);
  Result := UInt64ToStr(A) = S;
end;

function TestUInt64Div: Boolean;
var
  A0, A1, A2, A3, A4, A5, A6, A7, A8, B1, B2, B3, B4, B5, B6, B7, B8: TUInt64;
begin
  A0 := 0;
  A1 := TUInt64(4227372036857772807);
  A2 := TUInt64(-2227372036857772807); // 16219372036851778809
  A3 := TUInt64(97372037857779845);
  A4 := TUInt64(-97372037857779845);   // 18349372035851771771
  A5 := $22222222FFFFFFFF;
  A6 := $FFFFFFFF22222222;
  A7 := $FEFEFEFEFEFEFEFE;
  A8 := $FEFEFEFEFEFEFEFE;
  B1 := TUInt64(84560395435344);
  B2 := TUInt64(-684560395435342);     // 18446059513314116274
  B3 := TUInt64(-784560395435344);     // 18445959513314116272
  B4 := TUInt64(64560395435344);
  B5 := $1111111111111111;
  B6 := $1111111111111111;
  B7 := $0000000033333333;
  B8 := $3333333300000000;

  Result := (UInt64Div(A0, B1) = 0)
    and (UInt64Div(A1, B1) = 49992)
    and (UInt64Div(A2, B2) = 0)
    and (UInt64Div(A3, B3) = 0)
    and (UInt64Div(A4, B4) = 284220)
    and (UInt64Div(A5, B5) = 2)
    and (UInt64Div(A6, B6) = 14)
    and (UInt64Div(A7, B7) = 21390621439)
    and (UInt64Div(A8, B8) = 4);
end;

function TestUInt64Mod: Boolean;
var
  A0, A1, A2, A3, A4, A5, A6, A7, A8, B1, B2, B3, B4, B5, B6, B7, B8: TUInt64;
begin
  A0 := 0;
  A1 := TUInt64(4227372036857772807);
  A2 := TUInt64(-2227372036857772807); // 16219372036851778809
  A3 := TUInt64(97372037857779845);
  A4 := TUInt64(-97372037857779845);   // 18349372035851771771
  A5 := $22222222FFFFFFFF;
  A6 := $FFFFFFFF22222222;
  A7 := $FEFEFEFEFEFEFEFE;
  A8 := $FEFEFEFEFEFEFEFE;
  B1 := TUInt64(84560395435344);
  B2 := TUInt64(-684560395435342);     // 18446059513314116274
  B3 := TUInt64(-784560395435344);     // 18445959513314116272
  B4 := TUInt64(64560395435344);
  B5 := $1111111111111111;
  B6 := $1111111111111111;
  B7 := $0000000033333333;
  B8 := $3333333300000000;

  Result := (UInt64Mod(A0, B1) = 0)
    and (UInt64Mod(A1, B1) = 28748254055559)
    and (UInt64ToStr(UInt64Mod(A2, B2)) = '16219372036851778809')
    and (UInt64Mod(A3, B3) = 97372037857779845)
    and (UInt64Mod(A4, B4) = 16445218300091)
    and (UInt64Mod(A5, B5) = 3722304989)
    and (UInt64Mod(A6, B6) = 1229782934524998452)
    and (UInt64Mod(A7, B7) = 825307441)
    and (UInt64Mod(A8, B8) = 3617008645339807486);
end;

// =========================== Constant Time ===================================

function TestConstTimeSwap: Boolean;
var
  A8, B8: Byte;
  A16, B16: Word;
  A32, B32: Cardinal;
  A64, B64: TUInt64;
begin
  A8 := $02; B8 := $9F;
  ConstTimeConditionalSwap8(False, A8, B8);
  Result := (A8 = $02) and (B8 = $9F);

  if not Result then Exit;

  ConstTimeConditionalSwap8(True, A8, B8);
  Result := (A8 = $9F) and (B8 = $2);

  if not Result then Exit;

  A16 := $D280; B16 := $319B;
  ConstTimeConditionalSwap16(False, A16, B16);
  Result := (A16 = $D280) and (B16 = $319B);

  if not Result then Exit;

  ConstTimeConditionalSwap16(True, A16, B16);
  Result := (A16 = $319B) and (B16 = $D280);

  if not Result then Exit;

  A32 := $1D327806; B32 := $C379EB02;
  ConstTimeConditionalSwap32(False, A32, B32);
  Result := (A32 = $1D327806) and (B32 = $C379EB02);

  if not Result then Exit;

  ConstTimeConditionalSwap32(True, A32, B32);
  Result := (A32 = $C379EB02) and (B32 = $1D327806);

  if not Result then Exit;

  A64 := $2A64C05397B3C10D; B64 := $9C34A79E5B0F2180;
  ConstTimeConditionalSwap64(False, A64, B64);
{$IFDEF FPC}
  Result := (A64 = $2A64C05397B3C10D) and (B64 = TUInt64($9C34A79E5B0F2180));
{$ELSE}
  Result := (A64 = $2A64C05397B3C10D) and (B64 = $9C34A79E5B0F2180);
{$ENDIF}

  if not Result then Exit;

  ConstTimeConditionalSwap64(True, A64, B64);
{$IFDEF FPC}
  Result := (A64 = TUInt64($9C34A79E5B0F2180)) and (B64 = $2A64C05397B3C10D);
{$ELSE}
  Result := (A64 = $9C34A79E5B0F2180) and (B64 = $2A64C05397B3C10D);
{$ENDIF}
end;

function TestConstTimeSelect: Boolean;
var
  A8, B8: Byte;
  A16, B16: Word;
  A32, B32: Cardinal;
  A64, B64: TUInt64;
begin
  A8 := $02; B8 := $9F;
  Result := (ConstTimeConditionalSelect8(False, A8, B8) = B8)
    and (ConstTimeConditionalSelect8(True, A8, B8) = A8);

  if not Result then Exit;

  A16 := $D280; B16 := $319B;
  Result := (ConstTimeConditionalSelect16(False, A16, B16) = B16)
    and (ConstTimeConditionalSelect16(True, A16, B16) = A16);

  if not Result then Exit;

  A32 := $1D327806; B32 := $C379EB02;
  Result := (ConstTimeConditionalSelect32(False, A32, B32) = B32)
    and (ConstTimeConditionalSelect32(True, A32, B32) = A32);

  if not Result then Exit;

  A64 := $2A64C05397B3C10D; B64 := $9C34A79E5B0F2180;
  Result := (ConstTimeConditionalSelect64(False, A64, B64) = B64)
    and (ConstTimeConditionalSelect64(True, A64, B64) = A64);
end;

function TestConstTimeEqual: Boolean;
var
  A8, B8: Byte;
  A16, B16: Word;
  A32, B32: Cardinal;
  A64, B64: TUInt64;
begin
  Result := ConstTimeEqual8($09, $09) and ConstTimeEqual16($C32F, $C32F)
    and ConstTimeEqual32($7A8E6C1D, $7A8E6C1D) and ConstTimeEqual64($2A68C45397B3C10D, $2A68C45397B3C10D);

  if not Result then Exit;

  A8 := $02; B8 := $9F;
  A16 := $D280; B16 := $319B;
  A32 := $1D327806; B32 := $C379EB02;
  A64 := $2A64C05397B3C10D; B64 := $9C34A79E5B0F2180;
  Result := (not ConstTimeEqual8(A8, B8)) and (not ConstTimeEqual16(A16, B16))
    and (not ConstTimeEqual32(A32, B32)) and (not ConstTimeEqual64(A64, B64));
end;

function TestConstTimeExpandBool: Boolean;
begin
  Result := (ConstTimeExpandBoolean8(False) = 0)
    and (ConstTimeExpandBoolean16(False) = 0)
    and (ConstTimeExpandBoolean32(False) = 0)
    and (ConstTimeExpandBoolean64(False) = 0);

  if not Result then Exit;

  Result := (ConstTimeExpandBoolean8(True) = $FF)
    and (ConstTimeExpandBoolean16(True) = $FFFF)
    and (ConstTimeExpandBoolean32(True) = $FFFFFFFF)
    and (ConstTimeExpandBoolean64(True) = $FFFFFFFFFFFFFFFF);
end;

function TestConstTimeBytes: Boolean;
var
  A, B: TBytes;
begin
  A := HexToBytes('0987654321FBACDE');
  B := HexToBytes('0987654321FBACDE');
  Result := ConstTimeBytesEqual(A, B);

  if not Result then Exit;

  B[4] := $FF;
  Result := not ConstTimeBytesEqual(A, B);
end;

// ============================== Strings ======================================

function TestUtf8: Boolean;
const
  UTF16_LE_HEXSTR = '03546300610074006D993DD802DE42D8B7DF'; // 有单字节、二字节、四字节笑哭了表情符、四字节汉字上土下口
var
  L: Integer;
  W: WideString;
  Utf8: AnsiString;

begin
  L := HexToData(UTF16_LE_HEXSTR); // 得到字节长度
  SetLength(W, L div 2);           // 得到宽字符长度

  HexToData(UTF16_LE_HEXSTR, @W[1]);
  Utf8 := CnUtf8EncodeWideString(W);

  Result := AnsiStrToHex(Utf8) = 'E59083636174E9A5ADF09F9882F0A0AEB7';
  if not Result then Exit;

  W := CnUtf8DecodeToWideString(Utf8);
  Result := DataToHex(@W[1], Length(W) * SizeOf(WideChar)) = UTF16_LE_HEXSTR;
end;

// ============================== BigNumber ====================================

function TestBigNumberHex: Boolean;
const
  HEX_STR = '123321';
var
  T: TCnBigNumber;
begin
  T := BigNumberNew;
  T.SetHex(HEX_STR);
  Result := T.ToHex() = HEX_STR;
  BigNumberFree(T);
end;

function TestBigNumberDec: Boolean;
const
  DEC_STR = '240565850235271247637767721257294162758';
var
  T: TCnBigNumber;
begin
  T := BigNumberNew;
  T.SetDec(DEC_STR);
  Result := (T.ToDec() = DEC_STR);
  BigNumberFree(T);
end;

function TestBigNumberExpandWord: Boolean;
var
  T: TCnBigNumber;
begin
  T := BigNumberNew;
{$IFDEF CPU64BITS}
  if CnBigNumberIs64Mode then
  begin
    BigNumberWordExpand(T, 8);
    T.Top := 8;
    PCnBigNumberElementArray(T.D)^[0] := TCnBigNumberElement($0F73D4B9F147A700);
    PCnBigNumberElementArray(T.D)^[1] := TCnBigNumberElement($05D72BCFF78BBB54);
    PCnBigNumberElementArray(T.D)^[2] := TCnBigNumberElement($074D5382782E0E84);
    PCnBigNumberElementArray(T.D)^[3] := TCnBigNumberElement($07A20D1E34E475C2);
    PCnBigNumberElementArray(T.D)^[4] := TCnBigNumberElement($0CA4A192F7331A65);
    PCnBigNumberElementArray(T.D)^[5] := TCnBigNumberElement($0586C66DE2BD9685);
    PCnBigNumberElementArray(T.D)^[6] := TCnBigNumberElement($0BACACDE82782B14);
    PCnBigNumberElementArray(T.D)^[7] := TCnBigNumberElement($0F8DDBF39D15FB5B);

    Result := T.ToHex() = '0F8DDBF39D15FB5B0BACACDE82782B140586C66DE2BD96850CA4A192F7331A6507A20D1E34E475C2074D5382782E0E8405D72BCFF78BBB540F73D4B9F147A700';
  end
  else
{$ENDIF}
    Result := True;
  BigNumberFree(T);
end;

function TestBigNumberMulWord: Boolean;
var
  T: TCnBigNumber;
  W: TCnBigNumberElement;
begin
  T := BigNumberNew;
  T.SetHex('03094F68488B90DDBFC45B1129');
  W := 1000000000;
  BigNumberMulWord(T, W);
  Result := T.ToHex() = 'B4FB4C261C179660E6966CACA1345A00';
  BigNumberFree(T);
end;

function TestBigNumberModWord: Boolean;
var
  T: TCnBigNumber;
  W, R: TCnBigNumberElement;
begin
  T := BigNumberNew;
  try
    T.SetDec('111757582461903');
    W := 1;
    R := BigNumberModWord(T, W);
    Result := R = 0;

    if not Result then Exit;

    T.SetDec('111757582461902544929520711250223739903');
    W := 1000000000;
    R := BigNumberModWord(T, W);
    Result := R = 223739903;

    if not Result then Exit;

    T.SetHex('0C7D4FAEC98EC3DF');
    W := $6F6C929F;
    R := BigNumberModWord(T, W);
    Result := R = 1802899775;

    if not Result then Exit;

    T.SetDec('12345667296');
    W := 100000;
    R := BigNumberModWord(T, W); // Win32 下居然出错等于 0，后已修复
    Result := R = 67296;

    if not Result then Exit;

{$IFDEF CPU64BITS}
    T.SetDec('2345348872881627880943948657900100329812345667296');
    W := 1000000000;
    R := BigNumberModWord(T, W);
    Result := R = 345667296;
{$ENDIF}
  finally
    BigNumberFree(T);
  end;
end;

function TestBigNumberDivWord: Boolean;
var
  T: TCnBigNumber;
  W: TCnBigNumberElement;
begin
  T := BigNumberNew;
  T.SetDec('43246456');
  W := 1000000000;
  BigNumberDivWord(T, W);
  Result := T.IsZero;
  T.Free;
end;

function TestBigNumberUnsignedAdd: Boolean;
var
  A, B, R: TCnBigNumber;
begin
  A := BigNumberNew;
  B := BigNumberNew;
  R := BigNumberNew;
  A.SetHex('DC195D7B');
  B.SetHex('2D99AB36');
  BigNumberUnsignedAdd(R, A, B);
  Result := R.ToDec() = '4457695409';
  BigNumberFree(R);
  BigNumberFree(B);
  BigNumberFree(A);
end;

function TestBigNumberPowerMod: Boolean;
var
  A, B, C, R: TCnBigNumber;
begin
  A := BigNumberNew;
  B := BigNumberNew;
  C := BigNumberNew;
  R := BigNumberNew;

  A.SetHex('3D9967819913DFAE');
  B.SetHex('3B729AEF9BF48665');
  C.SetHex('76E535DF37E90CCB');
  BigNumberPowerMod(R, A, B, C);
  Result := R.ToHex() = '52A154E5CFCF5990';
  BigNumberFree(R);
  BigNumberFree(C);
  BigNumberFree(B);
  BigNumberFree(A);
end;

function TestBigNumberDiv: Boolean;
var
  A, B, C, R: TCnBigNumber;
begin
  A := BigNumberNew;
  B := BigNumberNew;
  C := BigNumberNew;
  R := BigNumberNew;

  A.SetHex('03910831DC05712D5BD3164D924AF751F5A51FABE9718F3E');
  B.SetHex('76E535DF37E90CCB');

  BigNumberDiv(R, C, A, B);
  Result := (R.ToHex() = '07ADE6030E1F606EC328070C769EEC15') and (C.ToHex() = '24A5D892043E5E97');

  BigNumberFree(R);
  BigNumberFree(C);
  BigNumberFree(B);
  BigNumberFree(A);
end;

function TestBigNumberRoundDiv: Boolean;
var
  A, B, C, R: TCnBigNumber;
  F: Boolean;
begin
  A := BigNumberNew;
  B := BigNumberNew;
  C := BigNumberNew;
  R := BigNumberNew;

  // 被除数和除数正负、负负、负正、正负，加上是四舍还是五入、除数奇偶，一共十六种组合情况
  A.SetDec('10005');
  B.SetDec('100');
  BigNumberRoundDiv(C, A, B, F);        // 100 5
  Result := (not F) and (C.ToDec() = '100');
  if not Result then Exit;

  A.SetDec('10050');
  B.SetDec('100');
  BigNumberRoundDiv(C, A, B, F);        // 100 50
  Result := F and (C.ToDec() = '101');
  if not Result then Exit;

  A.SetDec('-10005');
  B.SetDec('100');
  BigNumberRoundDiv(C, A, B, F);        // 100 -5
  Result := (not F) and (C.ToDec() = '-100');
  if not Result then Exit;

  A.SetDec('-10050');
  B.SetDec('100');
  BigNumberRoundDiv(C, A, B, F);        // 100 -50
  Result := F and (C.ToDec() = '-101');
  if not Result then Exit;

  A.SetDec('10005');
  B.SetDec('-100');
  BigNumberRoundDiv(C, A, B, F);        // -100 5
  Result := (not F) and (C.ToDec() = '-100');
  if not Result then Exit;

  A.SetDec('10050');
  B.SetDec('-100');
  BigNumberRoundDiv(C, A, B, F);        // -100 50
  Result := F and (C.ToDec() = '-101');
  if not Result then Exit;

  A.SetDec('-10005');
  B.SetDec('-100');
  BigNumberRoundDiv(C, A, B, F);        // 100 -5
  Result := (not F) and (C.ToDec() = '100');
  if not Result then Exit;

  A.SetDec('-10050');
  B.SetDec('-100');
  BigNumberRoundDiv(C, A, B, F);        // 100 -50
  Result := F and (C.ToDec() = '101');
  if not Result then Exit;

  // 以上除数是偶以下除数是奇
  A.SetDec('10048');
  B.SetDec('99');
  BigNumberRoundDiv(C, A, B, F);        // 101 49
  Result := (not F) and (C.ToDec() = '101');
  if not Result then Exit;

  A.SetDec('10049');
  B.SetDec('99');
  BigNumberRoundDiv(C, A, B, F);        // 101 50
  Result := F and (C.ToDec() = '102');
  if not Result then Exit;

  A.SetDec('-10048');
  B.SetDec('99');
  BigNumberRoundDiv(C, A, B, F);        // 101 -49
  Result := (not F) and (C.ToDec() = '-101');
  if not Result then Exit;

  A.SetDec('-10049');
  B.SetDec('99');
  BigNumberRoundDiv(C, A, B, F);        // 101 -50
  Result := F and (C.ToDec() = '-102');
  if not Result then Exit;

  A.SetDec('10048');
  B.SetDec('-99');
  BigNumberRoundDiv(C, A, B, F);        // -101 49
  Result := (not F) and (C.ToDec() = '-101');
  if not Result then Exit;

  A.SetDec('10049');
  B.SetDec('-99');
  BigNumberRoundDiv(C, A, B, F);        // -101 50
  Result := F and (C.ToDec() = '-102');
  if not Result then Exit;

  A.SetDec('-10048');
  B.SetDec('-99');
  BigNumberRoundDiv(C, A, B, F);        // 101 -49
  Result := (not F) and (C.ToDec() = '101');
  if not Result then Exit;

  A.SetDec('-10049');
  B.SetDec('-99');
  BigNumberRoundDiv(C, A, B, F);        // 101 -50
  Result := F and (C.ToDec() = '102');
  if not Result then Exit;

  BigNumberFree(R);
  BigNumberFree(C);
  BigNumberFree(B);
  BigNumberFree(A);
end;

function TestBigNumberShiftLeft: Boolean;
var
  A, B: TCnBigNumber;
begin
  A := BigNumberNew;
  B := BigNumberNew;

  A.SetHex('76E535DF37E90CCB');
  BigNumberShiftLeft(B, A, 3);
  Result := (B.ToHex() = '03B729AEF9BF486658');

  BigNumberFree(B);
  BigNumberFree(A);
end;

function TestBigNumberGetBitsCount: Boolean;
var
  A: TCnBigNumber;
begin
  A := BigNumberNew;
  A.SetHex('76E535DF37E90CCB');
  Result := A.GetBitsCount = 63;
  BigNumberFree(A);
end;

function TestBigNumberShiftRightOne: Boolean;
var
  A: TCnBigNumber;
begin
  A := BigNumberNew;
  A.SetHex('1F1BB7E73A2BF6B7175959BC04F056290B0D8CDBC57B2D0B19494325EE6634CA0F441A3C69C8EB840E9AA704F05C1D090BAE579FEF1776A91EE7A973E28F4E00');
  A.ShiftRightOne;
  Result := A.ToHex() = '0F8DDBF39D15FB5B8BACACDE02782B148586C66DE2BD96858CA4A192F7331A6507A20D1E34E475C2074D5382782E0E8485D72BCFF78BBB548F73D4B9F147A700';
  BigNumberFree(A);
end;

function TestBigNumberGcd: Boolean;
var
  A, B, C: TCnBigNumber;
begin
  A := BigNumberNew;
  B := BigNumberNew;
  C := BigNumberNew;

  // 要包括小范围的 Int64 的数以及大数以做到覆盖，因内部分开处理了
  A.SetDec('5');
  B.SetDec('13');
  BigNumberGcd(C, A, B);
  Result := C.ToDec = '1';
  if not Result then Exit;

  A.SetDec('625000');
  B.SetDec('10275');
  BigNumberGcd(C, A, B);
  Result := C.ToDec = '25';
  if not Result then Exit;

  A.SetDec('92752378340189327');
  B.SetDec('10098278758718');
  BigNumberGcd(C, A, B);
  Result := C.ToDec = '1';
  if not Result then Exit;

  A.SetDec('43674960102945789956341939450673822288888880');
  B.SetDec('98421092474268980864245077976421597415750');
  BigNumberGcd(C, A, B);
  Result := C.ToDec = '30';
  if not Result then Exit;

  BigNumberFree(C);
  BigNumberFree(B);
  BigNumberFree(A);
end;

function TestBigNumberLcm: Boolean;
var
  A, B, C: TCnBigNumber;
begin
  A := BigNumberNew;
  B := BigNumberNew;
  C := BigNumberNew;

  // 内部虽然没分开，但也推荐用小范围的 Int64 的数以及大数以做到覆盖
  A.SetDec('13');
  B.SetDec('5');
  BigNumberLcm(C, A, B);
  Result := C.ToDec = '65';
  if not Result then Exit;

  A.SetDec('93666666');
  B.SetDec('32784522');
  BigNumberLcm(C, A, B);
  Result := C.ToDec = '511802812023942';
  if not Result then Exit;

  A.SetDec('65536');
  B.SetDec('1609845068400000000');
  BigNumberLcm(C, A, B);
  Result := C.ToDec = '103030084377600000000';
  if not Result then Exit;

  A.SetDec('9681273465892781943786476838947823898923889185');
  B.SetDec('2387639409472541638495736283949590');
  BigNumberLcm(C, A, B);
  Result := C.ToDec = '1541026004069761890928432375060847439427168479412914472659863043376995685745610';
  if not Result then Exit;

  BigNumberFree(C);
  BigNumberFree(B);
  BigNumberFree(A);
end;

function TestBigNumberFermatCheckComposite: Boolean;
var
  A, B, C: TCnBigNumber;
begin
  A := BigNumberNew;
  B := BigNumberNew;
  C := BigNumberNew;

  A.SetHex('2BAF4FD43F390D534E60E6877676A17B63AD16893C6FB8D95B6E645BDF0FCB404C718563C903E9FA70985BCF19511BBCEF8D25E77843718BAA5B7A5B0975F242');
  B.SetHex('F8DDBF39D15FB5B8BACACDE02782B14C586C66DA2BD9685FCA4A192F7331A6537A20D1E34E475C2774D5382582E0E84D5D72BCFE78BBB54EF73D4B9B147A7001');
  C.SetHex('0F8DDBF39D15FB5B8BACACDE02782B14C586C66DA2BD9685FCA4A192F7331A6537A20D1E34E475C2774D5382582E0E84D5D72BCFE78BBB54EF73D4B9B147A7');

  Result := not BigNumberFermatCheckComposite(A, B, C, 12);

  BigNumberFree(C);
  BigNumberFree(B);
  BigNumberFree(A);
end;

function TestBigNumberIsProbablyPrime: Boolean;
var
  A: TCnBigNumber;
begin
  A := BigNumberNew;
  A.SetHex('F8DDBF39D15FB5B8BACACDE02782B14C586C66DA2BD9685FCA4A192F7331A6537A20D1E34E475C2774D5382582E0E84D5D72BCFE78BBB54EF73D4B9B147A7001');
  Result := BigNumberIsProbablyPrime(A);
  BigNumberFree(A);
end;

function TestBigNumberIsPerfectPower: Boolean;
var
  A: TCnBigNumber;
begin
  A := BigNumberNew;
  A.SetDec('9682651996416');
  Result := BigNumberIsPerfectPower(A);
  BigNumberFree(A);
end;

function TestBigNumberJacobiSymbol: Boolean;
var
  A, N: TCnBigNumber;
begin
  A := BigNumberNew;
  N := BigNumberNew;

  A.SetDec('3');
  N.SetDec('11');

  Result := BigNumberJacobiSymbol(A, N) = 1;
  if not Result then Exit;

  A.SetDec('59');
  N.SetDec('139');

  Result := BigNumberJacobiSymbol(A, N) = -1;
  if not Result then Exit;

  A.SetDec('8273190');
  N.SetDec('7143391235');

  Result := BigNumberJacobiSymbol(A, N) = 0;
  if not Result then Exit;

  A.SetDec('9237512893489267120909234987561230233241');
  N.SetDec('119872354839100272484348735687564378089401882467674327932479');

  Result := BigNumberJacobiSymbol(A, N) = -1;
  if not Result then Exit;

  A.SetDec('9237512893489267120909234987561230233241');
  N.SetDec('119872354839100272484348735687564378089401882467674327932479');

  Result := BigNumberJacobiSymbol(A, N) = -1;
  if not Result then Exit;

  A.SetDec('-1');
  N.SetDec('7');

  Result := BigNumberJacobiSymbol(A, N) = -1;
  if not Result then Exit;

  A.SetDec('-3');
  N.SetDec('19');

  Result := BigNumberJacobiSymbol(A, N) = 1;
  if not Result then Exit;

  A.SetDec('-7');
  N.SetDec('-37');

  Result := BigNumberJacobiSymbol(A, N) = -1;
  if not Result then Exit;

  A.SetDec('-15');
  N.SetDec('-21');

  Result := BigNumberJacobiSymbol(A, N) = 0;
  if not Result then Exit;

  BigNumberFree(N);
  BigNumberFree(A);
end;

function TestBigNumberMersennePrime: Boolean;
begin
  // 这些是梅森素数
  Result := BigNumberIsMersennePrime(2) and BigNumberIsMersennePrime(3) and
    BigNumberIsMersennePrime(5) and BigNumberIsMersennePrime(7) and
    BigNumberIsMersennePrime(13) and BigNumberIsMersennePrime(17) and
    BigNumberIsMersennePrime(19);

  if not Result then Exit;

  // 这些梅森不是素数
  Result := not BigNumberIsMersennePrime(4) and not BigNumberIsMersennePrime(8)
    and not BigNumberIsMersennePrime(11) and not BigNumberIsMersennePrime(15)
    and not BigNumberIsMersennePrime(18) and not BigNumberIsMersennePrime(21);;
end;

function TestBigNumberAKSIsPrime: Boolean;
begin
  // 这些是素数
  Result := BigNumberAKSIsPrime(2) and BigNumberAKSIsPrime(3) and BigNumberAKSIsPrime(5)
    and BigNumberAKSIsPrime(859) and BigNumberAKSIsPrime(2999) and BigNumberAKSIsPrime(16769023)
    and BigNumberAKSIsPrime(87178291199) and BigNumberAKSIsPrime(9223372036854775783);

  if not Result then Exit;

  // 这些不是素数
  Result := not BigNumberAKSIsPrime(6) and not BigNumberAKSIsPrime(125)
    and not BigNumberAKSIsPrime(9999999999) and not BigNumberAKSIsPrime(87178291200)
    and not BigNumberAKSIsPrime(24036584) and not BigNumberAKSIsPrime(9223372036854775784);
end;

function TestBigNumberBPSWIsPrime: Boolean;
var
  N: TCnBigNumber;
begin
  // 这些是素数
  Result := BigNumberBPSWIsPrime(2) and BigNumberBPSWIsPrime(3) and BigNumberBPSWIsPrime(5)
    and BigNumberBPSWIsPrime(859) and BigNumberBPSWIsPrime(2999) and BigNumberBPSWIsPrime(16769023)
    and BigNumberBPSWIsPrime(87178291199) and BigNumberBPSWIsPrime(9223372036854775783);

  if not Result then Exit;

  // 这些不是素数
  Result := not BigNumberBPSWIsPrime(6) and not BigNumberBPSWIsPrime(125)
    and not BigNumberBPSWIsPrime(9999999999) and not BigNumberBPSWIsPrime(87178291200)
    and not BigNumberBPSWIsPrime(24036584) and not BigNumberBPSWIsPrime(9223372036854775784);

  if not Result then Exit;

  N := TCnBigNumber.Create;
  N.SetDec('17958680315477529118359626974837716792693875094011614024011168510975569729310314' +
    '880849257372098399323281383080965010765274640234224256167493484359641041522147531619250' +
    '480871054471940913644503701423747232778359344650625273884644359540555918976207228041284' +
    '2514717420103698926250778354663659964058787962356617577');
  Result := BigNumberBPSWIsPrime(N);
  if not Result then Exit;

  N.SetDec('14969176467827120502498686286027013471395488404082070216831428287046164277671531' +
    '718906110319350798869642932334626497763724550748618776256325699077121330088529531311509' +
    '011733515213756078066269277069262905021429760084973349218408676900227626414013688181522' +
    '0489013547400320331396810226222993793410831538299353421');
  Result := BigNumberBPSWIsPrime(N);
  if not Result then Exit;

  N.Free;
end;

// ================================ Bits =======================================

function TestBitsEmpty: Boolean;
var
  B: TCnBitBuilder;
begin
  B := TCnBitBuilder.Create;
  B.AppendByte(0, False);
  Result := B.ToString = '';
  B.Free;
end;

function TestBitsAppend: Boolean;
var
  B: TCnBitBuilder;
begin
  B := TCnBitBuilder.Create;
  B.AppendByte($38, False);
  Result := B.ToString = '000111';
  if not Result then Exit;

  B.AppendBit(False);
  Result := B.ToString = '0001110';
  if not Result then Exit;

  B.Clear;
  B.AppendByte($EA);
  Result := B.ToString = '01010111';
  if not Result then Exit;

  B.AppendWord($9F3B);
  Result := B.ToString = '010101111101110011111001';
  if not Result then Exit;

  B.AppendByteRange($FE, 3);
  Result := B.ToString = '0101011111011100111110010111';
  if not Result then Exit;

  B.AppendDWord($12345678, False);
  Result := B.ToString = '010101111101110011111001011100011110011010100010110001001';
  B.Free;
end;

function TestBitsDelete: Boolean;
var
  B: TCnBitBuilder;
begin
  B := TCnBitBuilder.Create;
  B.AppendDWord($12345678, False);

  B.DeleteBits(0, 0); // 啥都不干
  B.DeleteBits(1, 3);
  Result := B.ToString = '01110011010100010110001001';
  if not Result then Exit;

  B.DeleteBits(0, 5);
  Result := B.ToString = '011010100010110001001';
  if not Result then Exit;

  B.DeleteBits(13, MaxInt);
  Result := B.ToString = '0110101000101';
  B.Free;
end;

// =============================== Int128 ======================================

function TestInt128Add: Boolean;
var
  A, B, R: TCnInt128;
begin
  A := StrToInt128('922337203685477580700');
  B := StrToInt128('10000');

  Int128Add(R, A, B);

  Result := Int128ToStr(R) = '922337203685477590700';
end;

function TestInt128Sub: Boolean;
var
  A, B, R: TCnInt128;
begin
  A := StrToInt128('-922337203685477580800');
  B := StrToInt128('-10000');

  Int128Sub(R, A, B);

  Result := Int128ToStr(R) = '-922337203685477570800';
end;

function TestInt128Mul: Boolean;
var
  A, B, R: TCnInt128;
begin
  A := StrToInt128('10000000000000000000000000');
  B := StrToInt128('8');

  Int128Mul(R, A, B);

  Result := Int128ToStr(R) = '80000000000000000000000000';
end;

function TestInt128DivMod: Boolean;
var
  A, B, R, M: TCnInt128;
begin
  A := StrToInt128('123459223372036854775807000');
  B := StrToInt128('10000');

  Int128DivMod(A, B, R, M);

  Result := (Int128ToStr(R) = '12345922337203685477580') and (Int128ToStr(M) = '7000');
end;

function TestUInt128Add: Boolean;
var
  A, B, R: TCnUInt128;
begin
  A := StrToUInt128('8937478937471844674407370955161500');
  B := StrToUInt128('1000');

  UInt128Add(R, A, B);

  Result := UInt128ToStr(R) = '8937478937471844674407370955162500';
end;

function TestUInt128Sub: Boolean;
var
  A, B, R: TCnUInt128;
begin
  A := StrToUInt128('324467474718446741844674407370955161600');
  B := StrToUInt128('1000');

  UInt128Sub(R, A, B);

  Result := UInt128ToStr(R) = '324467474718446741844674407370955160600';
end;

function TestUInt128Mul: Boolean;
var
  A, B, R: TCnUInt128;
begin
  A := StrToUInt128('100000000000000000000888888888');
  B := StrToUInt128('987654321');

  UInt128Mul(R, A, B);

  Result := UInt128ToStr(R) = '98765432100000000000877914951122085048';
end;

function TestUInt128DivMod: Boolean;
var
  A, B, R, M: TCnUInt128;
begin
  A := StrToUInt128('7370954431844674407370955161500');
  B := StrToUInt128('10000');

  UInt128DivMod(A, B, R, M);

  Result := (UInt128ToStr(R) = '737095443184467440737095516') and (UInt128ToStr(M) = '1500');
end;

// ============================= Polynomial ====================================

function TestBigNumberPolynomialGaloisPrimePowerModularInverse: Boolean;
var
  F, G, Fp, Fq, Ring: TCnBigNumberPolynomial;
  Root: TCnBigNumber;
begin
  // NTRU 多项式例子公开参数 N = 11, P = 3, Q = 32
  // 多项式最高次数 N - 1
  // 多项式 F 和 G 作为私钥，参数均为 -1 或 0 或 1
  F := TCnBigNumberPolynomial.Create([-1, 1, 1, 0, -1, 0, 1, 0, 0, 1, -1]);
  // -1+x+x^2-x^4+x^6+x^9-x^10

  G := TCnBigNumberPolynomial.Create([-1, 0, 1, 1, 0, 1, 0, 0, -1, 0, -1]);
  // -1+x^2+x^3+x^5-x^8-x^10

  Fp := TCnBigNumberPolynomial.Create;
  Fq := TCnBigNumberPolynomial.Create;

  Ring := TCnBigNumberPolynomial.Create;
  Ring.MaxDegree := 11;
  Ring[11].SetOne;
  Ring[0].SetOne;
  Ring[0].Negate;  // 多项式环为 x^n - 1

  Root := TCnBigNumber.Create;

  Root.SetWord(3);
  // 求 F 针对 3 与 x^11 - 1 的模逆多项式
  BigNumberPolynomialGaloisModularInverse(Fp, F, Ring, Root);
  Result := Fp.ToString = '2X^9+X^8+2X^7+X^5+2X^4+2X^3+2X+1';

  if not Result then Exit;

  Root.SetWord(2);
  // 求 F 针对 32 与 x^11 - 1 的模逆多项式
  BigNumberPolynomialGaloisPrimePowerModularInverse(Fq, F, Ring, Root, 5);
  Result := Fq.ToString = '30X^10+18X^9+20X^8+22X^7+16X^6+15X^5+4X^4+16X^3+6X^2+9X+5';

  Root.Free;
  Ring.Free;
  Fq.Free;
  Fp.Free;
  G.Free;
  F.Free;
end;

// ============================== Lattice ======================================

function TestNTRUHPS2048509: Boolean;
const
  CNPACK: AnsiString = 'CnPack';
var
  NTRU: TCnNTRU;
  Priv: TCnNTRUPrivateKey;
  Pub: TCnNTRUPublicKey;
  Data, En, De: TBytes;
begin
  NTRU := nil;
  Priv := nil;
  Pub := nil;

  try
    NTRU := TCnNTRU.Create(cnptHPS2048509);
    Priv := TCnNTRUPrivateKey.Create;
    Pub := TCnNTRUPublicKey.Create;

    NTRU.GenerateKeys(Priv, Pub);

    Data := AnsiToBytes(CNPACK);
    En := NTRU.EncryptBytes(Pub, Data);
    De := NTRU.DecryptBytes(Priv, En);

    // 解出的内容有后续 #0
    Result := StrComp(PAnsiChar(CNPACK), PAnsiChar(BytesToAnsi(De))) = 0;
  finally
    Pub.Free;
    Priv.Free;
    NTRU.Free;
  end;
end;

function TestNTRUHPS2048677: Boolean;
const
  CNPACK: AnsiString = 'CnPack';
var
  NTRU: TCnNTRU;
  Priv: TCnNTRUPrivateKey;
  Pub: TCnNTRUPublicKey;
  Data, En, De: TBytes;
begin
  NTRU := nil;
  Priv := nil;
  Pub := nil;

  try
    NTRU := TCnNTRU.Create(cnptHPS2048677);
    Priv := TCnNTRUPrivateKey.Create;
    Pub := TCnNTRUPublicKey.Create;

    NTRU.GenerateKeys(Priv, Pub);

    Data := AnsiToBytes(CNPACK);
    En := NTRU.EncryptBytes(Pub, Data);
    De := NTRU.DecryptBytes(Priv, En);

    // 解出的内容有后续 #0
    Result := StrComp(PAnsiChar(CNPACK), PAnsiChar(BytesToAnsi(De))) = 0;
  finally
    Pub.Free;
    Priv.Free;
    NTRU.Free;
  end;
end;

function TestNTRUHPS4096821: Boolean;
const
  CNPACK: AnsiString = 'CnPack';
var
  NTRU: TCnNTRU;
  Priv: TCnNTRUPrivateKey;
  Pub: TCnNTRUPublicKey;
  Data, En, De: TBytes;
begin
  NTRU := nil;
  Priv := nil;
  Pub := nil;

  try
    NTRU := TCnNTRU.Create(cnptHPS4096821);
    Priv := TCnNTRUPrivateKey.Create;
    Pub := TCnNTRUPublicKey.Create;

    NTRU.GenerateKeys(Priv, Pub);

    Data := AnsiToBytes(CNPACK);
    En := NTRU.EncryptBytes(Pub, Data);
    De := NTRU.DecryptBytes(Priv, En);

    // 解出的内容有后续 #0
    Result := StrComp(PAnsiChar(CNPACK), PAnsiChar(BytesToAnsi(De))) = 0;
  finally
    Pub.Free;
    Priv.Free;
    NTRU.Free;
  end;
end;

function TestMLKEM512KeyGen: Boolean;
const
  S_EK =
    'C4A019A8941B1C639C0C6698AAA051E3B9C78BF7A15005A92AB3C6AB30AE974682F517B96F743889' +
    '696F6F75BDDF6C9A588963AE04477FD52396C3B904459A3DAA39D5939F33817E8F20CD70860915F3' +
    'B59FB088FFF98C0AFAAEFE352344681A249BCD0529B3578B4123D35C88E14F67616B1E19B28D027A' +
    'B7988680CB3B1C257257D36FC3A2B81E18B1F918AB9EB650874135AAD478E3F5B34526856F81A3C3' +
    '2541781127F82304FE86BE29A70D489177FA5B9292882BDC58B5756043C558CC50D666D888C70762' +
    'C71FAA5A0014A1E573B080016BFB17172D8B1B563C64AC39B536D568DA1853D00968334C412A26AE' +
    '9272B0606A98C6199E96417A946B8780988445BC867050B5CA9904B6322DC059993068B02C09C9C9' +
    'C2B00E719F45FBAF183B02CA258C95239E5A4993376B7BC47A8D2751C75C59B3AAA90640379ACB0A' +
    '66B1ECA226D95E377CBC47B7210422BE338283404496D8A29C8553417E38818547ACB6213D789224' +
    '8BA7746626B562CB1209871A058A92E85248B37185DD3393D7C95C9C36159CD646A7713B47A60EF3' +
    '3825F4F7CCBBC07B4AF88C25A553D11C3D637859818387013806D5A24914FBC7E6D58977674598CC' +
    '57CF632148C6BFBE24878F318E1471B08EBC90DB01978A837A86C36C3ED14E69F7C771C32939B7AA' +
    'A28498A038B558F75779D5C2C0736003C91864206E52A296F949058069ADC15033251890309A3F94' +
    'D97A2716992C7745F8A991D10653E6F480F7A912C6619B0BD85926F8A2A9885112ABB1C3604EFA6B' +
    '84301A2ABD0A5D6FC29C8B16C98AFA817B41ACF90C9776D603FC7A6B8DF22377E26105D159D35C10' +
    '89ACAE7B7427DDB6B01E1CAE79FB8AFAA6917C16BB4DCA544B3CB121D00F4921CDF10BBF44375883' +
    '51623153C916908BD5824684DCAA5B4C44EE9B1063405F723CA60EB52E0B75B7BF67BFD6D34F5AA9' +
    '4657B3AD1BD4CC8D0566BA35C501A78BCF8183D5A52D2E5C4E606AC09C29ABE5A539A280AB838B66' +
    'A261BADE2028C1D1A9C352B4C6F1919BDA72C2C213D3EA9586C73E0E2C3486D9B199FAA0EA8B039E' +
    'E9C3EE3B8E1380CE3C0E2BA2E81C9745BD63054616AD78EC8AC8C44B08CC4A217CA320DE7476805E';
  S_DK =
    'B0618E21C291A7D7228F2B5624037B382628A90A89D0213D54E20CA6989BF887A5F0E03256F66026' +
    '4724A5518830B5BA681153C512454CC47A3CEC3E49F3B300A928B05316155C0CF2E069BD6084B52C' +
    '30422878064B6CB3535FB5485491E6874AA00D820770D0E55A6CAB40F3F4117E764EA43C62A39129' +
    '43B6096CC41FF87ACA0C140AA37A8C49170E355B60CBF16399A00508989A316CB99426BCC9B9054A' +
    'E7B46DD3B9CE703CA98C4E8FF6759D2006E66114027B92DCC40F9B2C7402A7C9EE7849955BA9FB35' +
    '543427701F4C9267F65CD37640265192EB085AA5F02DBEB7125D77145E9B1BDD330E2E687E50789F' +
    '9D814F4587C3030A5FF95724E62AA84490ACA3C2C76AEC24C29C82385597A9A4395506B21160BF53' +
    '60133BF709F016817AA5CA266A9556BAA4CA71050B0A71400B97AF4C07782711EC290ED3799C9196' +
    '7AA5E9000BC670F295B53DA415EF8BA631F178F98C747AFC5673D61FC6C280FEB37467D09C80257F' +
    'BCC02C6D14A48AFB72C7499AF802614D24352750CFD7956B9C77BF92B561AD4365E3666DCBE80337' +
    '93BEFCC9C5AB572838B4BF459CB1A9822D410B0C6CFB5CA55723FFA555A197657E9A7DE72BCB7699' +
    '5D2E0BAA71409301B07AAAD1A092C3020C1032628109D11C1D46F50A739CB63497BA48BBA556E5A1' +
    '7F629E1D965C356809A3456BEF572234064EE79B9D68169628D8AF01DB25B138B27C6129F493522E' +
    '40B81A91632CCC41B772CDC2211C10D07422272DD0967C9BB52C52832BA807C7642587C608A0748A' +
    '32B8766A2C3345AAB527700C49AF887582A049D490B053D34B9A9B038C63AD829969FA706C93B906' +
    '736A897C9C7187430CFC980116B73E018C7F3FB6420C8046EA86977FAA904597313555C470059422' +
    '5CA3C9B61F789A59CC350DEEE23D37D5A585672446560C6F38278C0C595B61258BA9430393B1171A' +
    'A24A530BA37999ED395FE2050A15D9AE74751177890137333B01850BD1535788946817EC1FC604AC' +
    '2C01C6E23720236BB329172AEFE096B5F93252DC6BB6D82C369AA14822AA92667578CA1238958DA7' +
    '2188D0BBA63DD7B6C4A019A8941B1C639C0C6698AAA051E3B9C78BF7A15005A92AB3C6AB30AE9746' +
    '82F517B96F743889696F6F75BDDF6C9A588963AE04477FD52396C3B904459A3DAA39D5939F33817E' +
    '8F20CD70860915F3B59FB088FFF98C0AFAAEFE352344681A249BCD0529B3578B4123D35C88E14F67' +
    '616B1E19B28D027AB7988680CB3B1C257257D36FC3A2B81E18B1F918AB9EB650874135AAD478E3F5' +
    'B34526856F81A3C32541781127F82304FE86BE29A70D489177FA5B9292882BDC58B5756043C558CC' +
    '50D666D888C70762C71FAA5A0014A1E573B080016BFB17172D8B1B563C64AC39B536D568DA1853D0' +
    '0968334C412A26AE9272B0606A98C6199E96417A946B8780988445BC867050B5CA9904B6322DC059' +
    '993068B02C09C9C9C2B00E719F45FBAF183B02CA258C95239E5A4993376B7BC47A8D2751C75C59B3' +
    'AAA90640379ACB0A66B1ECA226D95E377CBC47B7210422BE338283404496D8A29C8553417E388185' +
    '47ACB6213D7892248BA7746626B562CB1209871A058A92E85248B37185DD3393D7C95C9C36159CD6' +
    '46A7713B47A60EF33825F4F7CCBBC07B4AF88C25A553D11C3D637859818387013806D5A24914FBC7' +
    'E6D58977674598CC57CF632148C6BFBE24878F318E1471B08EBC90DB01978A837A86C36C3ED14E69' +
    'F7C771C32939B7AAA28498A038B558F75779D5C2C0736003C91864206E52A296F949058069ADC150' +
    '33251890309A3F94D97A2716992C7745F8A991D10653E6F480F7A912C6619B0BD85926F8A2A98851' +
    '12ABB1C3604EFA6B84301A2ABD0A5D6FC29C8B16C98AFA817B41ACF90C9776D603FC7A6B8DF22377' +
    'E26105D159D35C1089ACAE7B7427DDB6B01E1CAE79FB8AFAA6917C16BB4DCA544B3CB121D00F4921' +
    'CDF10BBF4437588351623153C916908BD5824684DCAA5B4C44EE9B1063405F723CA60EB52E0B75B7' +
    'BF67BFD6D34F5AA94657B3AD1BD4CC8D0566BA35C501A78BCF8183D5A52D2E5C4E606AC09C29ABE5' +
    'A539A280AB838B66A261BADE2028C1D1A9C352B4C6F1919BDA72C2C213D3EA9586C73E0E2C3486D9' +
    'B199FAA0EA8B039EE9C3EE3B8E1380CE3C0E2BA2E81C9745BD63054616AD78EC8AC8C44B08CC4A21' +
    '7CA320DE7476805EA1CBE5BF3592B77207A8C0C8E3FAA7EF66DEB7C18CE8CCF387A76B39CB4ECB72' +
    '3764A9B01F8C411BEB6DACF298CC30841FEB65A10D6DEBA9780338B4970B983E';
var
  M: TCnMLKEM;
  EK, DK: TBytes;
begin
  M := TCnMLKEM.Create(cmkt512);
  M.GenerateKeys(EK, DK, '00C7E4AB727C202A92869BE37F8A7CBAE63A67463C3A54000F1A3290B987E96F',
    '3764A9B01F8C411BEB6DACF298CC30841FEB65A10D6DEBA9780338B4970B983E');
  Result := (BytesToHex(EK) = S_EK) and (BytesToHex(DK) = S_DK);
  M.Free;
end;

function TestMLKEM512KeyEncapDecap: Boolean;
const
  S_EK =
    'CE22B227E25D5B5422E53ABD78D6B80AF94A4BFC15AC6A39C56602C1B29934F456E23C9FB9BA568E' +
    '4C2B3E7B8D6AF7A8AEB4A7AC94A57453CE6E05AC505B1F6785BB024B6EA9744E6C7874472AAFFF7B' +
    '0F58A5ABFD2424F9E3B8AB9A4813A19FCCD2CE35E1C45829BB5888A9D6C14E351618F72B9135FA06' +
    '77B91FD0EB84F997C5FFE7C45FB03067238FB5C880469079CB13620DEBB9A4E5247E08247B1C5886' +
    '66CE43EB6377C33741F2372CD14FC9D6BD9F53997FD92A1059AD5FC261EE89561AD26EB7C88A9537' +
    '0CFEA4A76A9CB5DF32996E5C060AA1CB8F39B8AC9956BD037AE3C750F255AED4B56E44317ABAC044' +
    '2BF484F43896B2493A53568A6123444FC50E6A3B81A6223BE973902CD64C03F233B0B638538B57BE' +
    '5A43F11BA31B93CF15D32844E48AAFEA0DB277A19E9A395496366D9C9AD2341BE34C58941692A1DC' +
    '7570626202A5185903466CEA91B7078C357787967493569760C348ABF7C223EAB1365F455C85306E' +
    'CF8BB951C0727FB94B89790A75D03782B568589B194A80C049DA498347C50B26447F5C8334B70C9E' +
    'F74EB65C1DA2429B11942D0D57BB14415C6442973F079B1881AD9D5C71657757AB2A2650EC252292' +
    'ADD1AC2967B9CA1158A6D2E5BBD55211D24874753CC673A8A689B8021010BFA2A8938F3B6F68549C' +
    '326A221EB86BF7D98A8ED708BBC95AA6D51F8CF94ED374A1C0585F36BC7F22CBA513449942363F37' +
    'D167CFCA6A38588147F101E26B5F818C2C67B515D7802DF57437BE9567D77A5164B4A061AA0EF019' +
    'CF1E9024FAC61F43812B47846185C4BA2B4A7A3CD51CDACB558DF8101C2326EA8B4313C91C043542' +
    'D7B53EF8B83AE02AAEDA2749B2913CC65487D408C463233750A4CF3913957F270753745CA8450BBD' +
    '113877C3A9D370643D5BC13D0B4D6D979C9BC197BCA91EA0DC004575C7F8CCC08DA501356A4197AB' +
    '34F34554DD461FD9C3A94A37C90FB864BAEC31D26B7E0D83853C48C896018D67113C4BD723362B87' +
    'DA964C56027E820B6412282BA7A5819E7A9E4DD27E8C778B8EB8AC7C21BEFEA4192A9468BFC13BC0' +
    '8B079042152012250A0E9261D5B0B540A09464A54EF02AD0F1255ADC57F034529127DA186DCE9527';
  S_DK =
    'D910660105C412B3698D043171C1CC9AF2CC65AA019765034FD94AB6F11613DBB80223371970AB05' +
    'EB3F325686B6A771EE3324DB5BCD32726B0BC760FFDC486AE473D31521E3EB8B5B5A9E0E7A1DA810' +
    '4E39752D2A32BC08130F2902C8ED33C8A9E6A670AAB329D170A171AE7450686A0CA980BB8C4203AF' +
    'CC3985C5E5130AE598EFBA572C316149FC9AD4774FC154A7E12662389849C27A64B0B4A53193A83F' +
    '50AD6A3A4BEC9A5707F09FBEAA360DD1837E217E1AF335DBC120B5C61FC5260B81408AB9793C431C' +
    'CB0C9624FDBB687B0699DBC93445363E7A4BA9A644630E4B9B1D03BFB7DCC18AD3182E6891D33927' +
    'FB9188739043D774C06830C48E84286C817F4259340A366D4EE0141917924551C822ACBEBE57BBDF' +
    'F58774E780F82B9E4F375CA1D14C344A401759A0D8277494EBCA2955753D7B4A01B3B864E66ED2B6' +
    '44EBA073F8805F8E07208799871E7777EFF5A539AA57D3C55AA9844C3EEA456EF5877A5C1F637993' +
    'C1021307F8C97CD1165AC837078877691B0B8B7B05295C8F7E7C15406636E18B9E3E98C1F0C522A2' +
    '92BE59B974939C293158290A503BD84456E73351C6495284F46FDA0544D9BCBB54189658975AE1E5' +
    'BBF4F0410FE3355A33AAABD6335026B55CB2CF2B21CA581BB8A2055EDC918BA3570128E392BF10C5' +
    '5A0BCB97C977ABDB8784253908900D6CEC33B2428FEBE6A426274120C27ABF1AC0FB81C622DA0DF7' +
    '972E201A28D788A761D9242E63B68A7A7B2E67840BF76382F2159E2C425CA69BB1FAB1FEE66AA0B8' +
    '9E4AB8A2781649676124301CAB6BC4B790B1C26AF36C90B2691C50157D153424AC22444B13BF297A' +
    'EFFCA74196B2F6A7456A80213B381C6D776296A7173B6A28CE16C41AFA657C7911EAD7354F2770C8' +
    'FB1E06878841738B51C95371D690A8BBA1D5AB098B9041F2A3B9EAF07DF6DC315DA273BF0503DAC3' +
    '190B27A0CEC78E3FC4456E51C9DCB65C702A777FB0727266AEF47B2A7EF61BA475B60CB2CC30C039' +
    'D3CC87A05AA72A252EE043CBFB173BA13015BF08C382CAAAC1367CEE779D0AE6BA0F5435B9241496' +
    'A581ACB36C3ED212CE22B227E25D5B5422E53ABD78D6B80AF94A4BFC15AC6A39C56602C1B29934F4' +
    '56E23C9FB9BA568E4C2B3E7B8D6AF7A8AEB4A7AC94A57453CE6E05AC505B1F6785BB024B6EA9744E' +
    '6C7874472AAFFF7B0F58A5ABFD2424F9E3B8AB9A4813A19FCCD2CE35E1C45829BB5888A9D6C14E35' +
    '1618F72B9135FA0677B91FD0EB84F997C5FFE7C45FB03067238FB5C880469079CB13620DEBB9A4E5' +
    '247E08247B1C588666CE43EB6377C33741F2372CD14FC9D6BD9F53997FD92A1059AD5FC261EE8956' +
    '1AD26EB7C88A95370CFEA4A76A9CB5DF32996E5C060AA1CB8F39B8AC9956BD037AE3C750F255AED4' +
    'B56E44317ABAC0442BF484F43896B2493A53568A6123444FC50E6A3B81A6223BE973902CD64C03F2' +
    '33B0B638538B57BE5A43F11BA31B93CF15D32844E48AAFEA0DB277A19E9A395496366D9C9AD2341B' +
    'E34C58941692A1DC7570626202A5185903466CEA91B7078C357787967493569760C348ABF7C223EA' +
    'B1365F455C85306ECF8BB951C0727FB94B89790A75D03782B568589B194A80C049DA498347C50B26' +
    '447F5C8334B70C9EF74EB65C1DA2429B11942D0D57BB14415C6442973F079B1881AD9D5C71657757' +
    'AB2A2650EC252292ADD1AC2967B9CA1158A6D2E5BBD55211D24874753CC673A8A689B8021010BFA2' +
    'A8938F3B6F68549C326A221EB86BF7D98A8ED708BBC95AA6D51F8CF94ED374A1C0585F36BC7F22CB' +
    'A513449942363F37D167CFCA6A38588147F101E26B5F818C2C67B515D7802DF57437BE9567D77A51' +
    '64B4A061AA0EF019CF1E9024FAC61F43812B47846185C4BA2B4A7A3CD51CDACB558DF8101C2326EA' +
    '8B4313C91C043542D7B53EF8B83AE02AAEDA2749B2913CC65487D408C463233750A4CF3913957F27' +
    '0753745CA8450BBD113877C3A9D370643D5BC13D0B4D6D979C9BC197BCA91EA0DC004575C7F8CCC0' +
    '8DA501356A4197AB34F34554DD461FD9C3A94A37C90FB864BAEC31D26B7E0D83853C48C896018D67' +
    '113C4BD723362B87DA964C56027E820B6412282BA7A5819E7A9E4DD27E8C778B8EB8AC7C21BEFEA4' +
    '192A9468BFC13BC08B079042152012250A0E9261D5B0B540A09464A54EF02AD0F1255ADC57F03452' +
    '9127DA186DCE95274FF27A9AF1C9074D3439797A96CC512AEE5D09BEA36A6BAE4C5D1A75943D02C5' +
    '6290C092088A99E4C5A6ED16C739BCBF33E640CE33D7DE20714305EF61BA2B96';
  S_CT =
    'C3D565942112A5B216850124DF528E6792119D688EC12BB9438F83AAF57DD05B1AD9DBDDF482949B' +
    'B880E4F76FE77AAEB0946B4A5F5A2BBCC8E5C055360B765049D57411E2D3DC201611BEB7B665B7F9' +
    '2453245A668147D1308DE2991F57581E207FA8860890FE3CDB97E35482C38C2761C1CFBF1075F2DA' +
    '1EEF3B93EBB98B448780183D61E1A70E6EC407669B3734907C5212C6915A368535F6D0DD5C31B0C0' +
    '800E3374B69B53D2E497BCA88EA5E8A4266A31DDA6D263F7888A85E5E648E79862F0C5B4CDBBD4DA' +
    'D72D758532553569E16EA6C73FC2F82B65D074F710663E5D17A5FAD1B05152F05E9005714CB39591' +
    '8E59B275BD90B6B9AE588C7984B481E459CDBFBC0E00F0422C7C8EECC783EA6CA1BEAF970E6BBCD5' +
    '4061210FF24FE504D9756C79BF746531EC958244623F89ED87AB48B6CCF28BA18E43298B7E5C3F3E' +
    'C2E3B669F427762375DA779D206CC889337429DF1A1AD02E0268B8F9F479A573067C9493C1A65CDF' +
    'A7E7DD2FFA7001423D39C0AFF53910C404E2334420493395EF2C8C2E1F01D2D2F82F7DB8D2FCD30A' +
    'CD1679D943A9E705AC35F9D10BA9464D6EC52FF86610DE36C79B639B5634776F68203A1D6FC8FD89' +
    '2BDE9F59978C35446C0D4A684108ECCFFC631FD09031932C7435C5EA2E89A25FC08F0196980F2B94' +
    '0CD4D4429AE195ACF14AD8042C09D39A3EE7DC1D1344A8BF5A530BF7C7270482E408FE33E6584C82' +
    '6D2495A5379948F4E90CEACA48757572B086A02AE005EB238C28EFA1D3E9343866C2B5865565FED0' +
    '1CC54D1B1C16148435B0B39F73676FBA06D0FD57B287CA05C7FD0F97B69E62B1A8B2457EBBC8CC4C' +
    'AAC82EE1A7FDC30649DEDC7129146C6E44E1EF1CCE2BE004E13A276F3E06E8D2801D3EC2616C81CE' +
    'C6439C9A3827337E20705BAF6F30941673CC8E773188333A76C1C2B04F2D259BB61AEC2B72F846F1' +
    'C7B2D50582D641F49E61AB4F0828DC732676C3208123F990B9384364A9607905FF155E0107B51624' +
    'ABACF1DCF48EB93A39E117D9B802EAFD59732D6D7E94E9C7F33AB68908DC87ACAF80EF4BEEA4CCB5' +
    '35C3559519A7E81C';
var
  M: TCnMLKEM;
  EK, DK, SK, CT: TBytes;
begin
  M := TCnMLKEM.Create(cmkt512);

  try
    EK := HexToBytes(S_EK);
    DK := HexToBytes(S_DK);

    M.MLKEMEncaps(EK, HexToBytes('971B2D3905A306927C18F0F72D9F58CD3FAC6238C902C3AE679449F6CDB91235'), SK, CT);
    Result := (BytesToHex(SK) = 'F833AEC5841D7F3CA0B188221245745D66B374ED8C3C04AE3522ADE7F973F8D5') and
      (BytesToHex(CT) = S_CT);

    if not Result then Exit;

    SK := M.MLKEMDecaps(DK, CT);
    Result := BytesToHex(SK) = 'F833AEC5841D7F3CA0B188221245745D66B374ED8C3C04AE3522ADE7F973F8D5';
  finally
    M.Free;
  end;
end;

function TestMLKEM768KeyGen: Boolean;
const
  S_EK =
    '5219C4CC17C35A828F3E21B2AB7496805C99EE041FCA0158A3314F07D053F364C887A6825958A625' +
    '965D4885C2CB355E83A3C1BBB15446F891D2D24F145632CF06A5EE1A278CD3064A79AD53193853E4' +
    'CEA654448A4297CEA3C9E87561629680F588953B858074292ED31C20DDD983E805D07BB9CFADD823' +
    'C7900B604286C0184738CA04E0DA8289540E329605EFAA5960AAC0FD0760006C1F1993426CC7BEA2' +
    '2BCBB3CC02E099B828E82F94045DFACB1D9FB315582B20D1B41476FC43AC4680647259FE9B513712' +
    '23446C82E0BBAEA132913E2B96EA11950C450F25854EE4FA4921193C8F1C66D61B8265C7072B046F' +
    '0C532141D51D9919C80733C1BD3C5A6D77CCB3A1938C95C1E4E866D1D65C78297B3B32CA3C4143E6' +
    'A215C609A36A1B13BAA17981D42B7FF4C715AC806DC491560032A5A2BB30E476A266C6E4A4A065D9' +
    '698DB08132608136082689B1B648B49063C98324706A43876507FC690893900F8166E1D52ADCC448' +
    '48D864B8BA0933B32DAA63435F11065915C5D5D879EDCC136FA8515B0260B9536C316120B4904921' +
    '805521C0232DA126E2A9C5323976ABC73B5AB892E59B01A194B6446C1B73217FBBA855AC887AFA58' +
    'A8F15A768EF968D775267E050150C7A8237C1024F9421C210D97907B2A144736E3B58E01C48947B1' +
    'C62655E380256491CEBE400F52493D9033A29D6C9AE80B33E8C38584B30A31A37B15E85F5E82B731' +
    '57408A5399A957190CBA905F1713C9ACA53AA32923CACB8269436A56BC02932189C7139D8463D0DC' +
    '540621875E9A7DFEF020E7E83696C7612BC2A7DE7148D7075C31F257766301FA5A06E00582DCE15C' +
    '6FB3195AF43078D79B110B5DC0789BE3A32132737FEB247DF8D2216F272C3D5BA700C52ED7E3A279' +
    '5CCFE3072CD1D1B533939AC58A065A8A9FE85441ED291D43869E25307737A5155A095C5056A20432' +
    '6F944A9FC8F7035C931A61033CC2DB6ECEC08AAE0045A6542080C1A7BBD699902A2ECBE3A1F9AA95' +
    'FC6222C6C6AB7BF96EDB3450E029B0CE104DBBFA0A49B8044EE1BB63B33BCFAA8A6A450194CC577D' +
    '490A549B6C42D6816F444AAE8B8A4F62AFFD17664E957E2BA245A864BAB8D28E82CAB9FAC3BCD707' +
    'AAC19A5EE58A3925DC7059BA1B9E7A798EC987E1924342C54E5D935A6699B6D9AA78E4738B0CEC6C' +
    '57D8228C86881A14A3B57065BBE5653BD92B5BD5A9888B1B1E9B017ECA1A788CBC3A283A9F7B00D3' +
    'F55B138CA26C33CD2CA3BA632A0735596162AA6C9D10968F53305D35CEC4395CA0489FF631A3EF66' +
    '1264A22ACB535DCC1313CD6917BFE3B5B8FC3B8016CA0DFB151623C92F95701DCC4140459B52EB79' +
    '6FDA60FB429651052D2C16B550FA035CC1374C87A439077713769C728B766BC75A609B70BBC14AF8' +
    '4A3C70519C3211ACAD58BF7C14C15F1AA86FCC1DF55580A2F9BBBC31B6B0EAAB379481CE9966F7FA' +
    '6487C611BA5B3E8F8307FA35CD5248C8AB351B63ABC4BA005871A97751F01E5143302C757A65E411' +
    'AF7B26F22076C7A1CF7E2B0055E80D7116CA2B3056BF8754BCFA9095D0CE99715CC4512F10125C5A' +
    '500DABB7C11F5B0408377900621BE851B7A3602576650B84191749F30AC635A9E2212400769DE8E1' +
    '915F26BA198AF8E53DACB598711738DA8C583A388E027A59';
  S_DK =
    'E6F634F0A3771ED789D6842321E147B6010ADA7B6B0B105949F90AEBECCA062C494743AD3BE35A23' +
    'C9BAA0E4CB83E65C9EB8BBC42433F0AC671ED9544A37C0CD704D1C24343ACA7BEBB538C7051186BC' +
    '624A9A98F536874C3A15E145732A591ECE600C78028426BB8A7AF22757296EB968B5E3553721DB6B' +
    'D507A5BE5742F45AA3E134383499400BDAC0C8607FAD647F41A2B547A631F971224C93570141C844' +
    '662F96D9A3A32B6DB1C93F2A2B82E6B370B7D91EB9F9B8D551C8C2F3CEA6C2711379969467ABF704' +
    '1848BB05E619A7B63CC2F9DAC97C05457911470EC7B4CD380528D695604B6F042394524C5C77415C' +
    '3CA15FA64A9E16B240ED12CFF0A48AF72743916C469593B89D8510BFEC58A0532948D80D32A11FCE' +
    '265ACBE6C21751507FD72366EB5EDD650344268757A715F27B28BEB4CF80347DCFCA34BB92A04695' +
    '5AA39590C2397A223ACB42B94B938329CB6A9C4253137531C54D2100966B9A8C96110A412AD2DC44' +
    '80FAB04D05B93087AE8E4A1616526A48F38CAA920C6F495CA9E488776551630010C46C375DE52AA5' +
    'EC23CCE9BCCE6663F1678B9F4325D1A961D551A02B046320F52272382DA8D565563A4CAC7419830B' +
    '6C5F95629B3441727CCCB4892BA765B212221147076872361A0A31AA7C50403976042DA5486D287D' +
    '143083646A90C6F1CA4037868E8478952829FD4C7E84187894F28CCD58170CF107FE74AF5A0CCFC8' +
    '49A71EEA4860080100310259FAA59C477C7002BD93385B8DA14E57891585253AE0CAA66BC3A33F05' +
    '2512788828916F0AC3C9C4FA6E31AC4088BC6A47F29386E467FEA414EDFA9194D30CE0770D3ACB1C' +
    '1162312ACC64FF2793A2AC4E990726D1739E467A4BA8221169137AD1069EED5112EFF648F2353CCA' +
    '4B4F8469CD29D05F2684236AA3AB3B019E6635AB53AC456D2C9D83152554655F8D6B4F75E2387067' +
    '7F492A2BBA53913B535A0B54113193B4122A5331585EDCC42C9361A08C173F73E4182F85B15DFC6F' +
    '795269C88A19C4FB7D7D33AEA8144000827C57E73AFE87C6D9B646B1B1BE560672C858531D835BC3' +
    'DA21B1407DB74A9D6B729F5BB6CA1AD086F3C2461D78A29A21238B0434594058BCDB125EE56F3B60' +
    '6D8C7A17CA5C3D6F876A3061BA5B65B97F033D3889AA74302D1EA268531354A707756EB60E621AC9' +
    '59E5AB5875790E6477D5C91A3C06593B976010F054AC6B4D54D452E5302E8E332CA6457CF2527DDF' +
    'CC3B5462C7171105F9E24C8D299CA348CE9DB678E041A135893E16AC9CCB2BACCC202F7BF7B3DB1C' +
    '8C5D567FE3E3C233024C75D369D7CC1093B2C2EC9094D020A8782ABFBFB89FF199517B0A4BB81C74' +
    '55A64D9C07897877422D6ACCEEFA16EA242E0CEC8B5EF20C85E82E2DCAC3A3030B211A600B520B7D' +
    '66B58AC317B0F65518F20EF26AB5B7D8AA729210A92B12FF115B43540509156CC247B4BCB34C313B' +
    '3A38228D4BACB7E1E934FD40CB42322B6E618670E17A725CB37CB20210E000678393DE6A5D40786C' +
    'BE019DAEF867F2A0B8F1827F48C0C04305357B82BAB80CC144A6997228A846D9CB49E32B865858A7' +
    '601F55CB8127DA0BEA4437F017502AC8709E428309F6A252C69042C973BA104A5219C4CC17C35A82' +
    '8F3E21B2AB7496805C99EE041FCA0158A3314F07D053F364C887A6825958A625965D4885C2CB355E' +
    '83A3C1BBB15446F891D2D24F145632CF06A5EE1A278CD3064A79AD53193853E4CEA654448A4297CE' +
    'A3C9E87561629680F588953B858074292ED31C20DDD983E805D07BB9CFADD823C7900B604286C018' +
    '4738CA04E0DA8289540E329605EFAA5960AAC0FD0760006C1F1993426CC7BEA22BCBB3CC02E099B8' +
    '28E82F94045DFACB1D9FB315582B20D1B41476FC43AC4680647259FE9B51371223446C82E0BBAEA1' +
    '32913E2B96EA11950C450F25854EE4FA4921193C8F1C66D61B8265C7072B046F0C532141D51D9919' +
    'C80733C1BD3C5A6D77CCB3A1938C95C1E4E866D1D65C78297B3B32CA3C4143E6A215C609A36A1B13' +
    'BAA17981D42B7FF4C715AC806DC491560032A5A2BB30E476A266C6E4A4A065D9698DB08132608136' +
    '082689B1B648B49063C98324706A43876507FC690893900F8166E1D52ADCC44848D864B8BA0933B3' +
    '2DAA63435F11065915C5D5D879EDCC136FA8515B0260B9536C316120B4904921805521C0232DA126' +
    'E2A9C5323976ABC73B5AB892E59B01A194B6446C1B73217FBBA855AC887AFA58A8F15A768EF968D7' +
    '75267E050150C7A8237C1024F9421C210D97907B2A144736E3B58E01C48947B1C62655E380256491' +
    'CEBE400F52493D9033A29D6C9AE80B33E8C38584B30A31A37B15E85F5E82B73157408A5399A95719' +
    '0CBA905F1713C9ACA53AA32923CACB8269436A56BC02932189C7139D8463D0DC540621875E9A7DFE' +
    'F020E7E83696C7612BC2A7DE7148D7075C31F257766301FA5A06E00582DCE15C6FB3195AF43078D7' +
    '9B110B5DC0789BE3A32132737FEB247DF8D2216F272C3D5BA700C52ED7E3A2795CCFE3072CD1D1B5' +
    '33939AC58A065A8A9FE85441ED291D43869E25307737A5155A095C5056A204326F944A9FC8F7035C' +
    '931A61033CC2DB6ECEC08AAE0045A6542080C1A7BBD699902A2ECBE3A1F9AA95FC6222C6C6AB7BF9' +
    '6EDB3450E029B0CE104DBBFA0A49B8044EE1BB63B33BCFAA8A6A450194CC577D490A549B6C42D681' +
    '6F444AAE8B8A4F62AFFD17664E957E2BA245A864BAB8D28E82CAB9FAC3BCD707AAC19A5EE58A3925' +
    'DC7059BA1B9E7A798EC987E1924342C54E5D935A6699B6D9AA78E4738B0CEC6C57D8228C86881A14' +
    'A3B57065BBE5653BD92B5BD5A9888B1B1E9B017ECA1A788CBC3A283A9F7B00D3F55B138CA26C33CD' +
    '2CA3BA632A0735596162AA6C9D10968F53305D35CEC4395CA0489FF631A3EF661264A22ACB535DCC' +
    '1313CD6917BFE3B5B8FC3B8016CA0DFB151623C92F95701DCC4140459B52EB796FDA60FB42965105' +
    '2D2C16B550FA035CC1374C87A439077713769C728B766BC75A609B70BBC14AF84A3C70519C3211AC' +
    'AD58BF7C14C15F1AA86FCC1DF55580A2F9BBBC31B6B0EAAB379481CE9966F7FA6487C611BA5B3E8F' +
    '8307FA35CD5248C8AB351B63ABC4BA005871A97751F01E5143302C757A65E411AF7B26F22076C7A1' +
    'CF7E2B0055E80D7116CA2B3056BF8754BCFA9095D0CE99715CC4512F10125C5A500DABB7C11F5B04' +
    '08377900621BE851B7A3602576650B84191749F30AC635A9E2212400769DE8E1915F26BA198AF8E5' +
    '3DACB598711738DA8C583A388E027A597CA0C2CBBF4FBF28DE8C479D4473C339D96B89C34A4E5FCB' +
    'CF7728BDFB43B945D6BF055CB7B375E3271ED131F1BA31F83FEF533A239878A71074578B891265D1';
var
  M: TCnMLKEM;
  EK, DK: TBytes;
begin
  M := TCnMLKEM.Create(cmkt768);
  M.GenerateKeys(EK, DK, 'A2B4BCA315A6EA4600B4A316E09A2578AA1E8BCE919C8DF3A96C71C843F5B38B',
    'D6BF055CB7B375E3271ED131F1BA31F83FEF533A239878A71074578B891265D1');
  Result := (BytesToHex(EK) = S_EK) and (BytesToHex(DK) = S_DK);
  M.Free;
end;

function TestMLKEM768KeyEncapDecap: Boolean;
const
  S_EK =
    '62387A619993B5D9567CE83676994DC54C46D6E380A8944E0DC60DD6312460E698CFA6AEC0AB440C' +
    '3A3D93F21F65A05B42933CA538108BE5235F8A69B505CC2ED84CC0606EEC5B1449BC5F12048074F0' +
    '5985429AFB6C9FBB7140C28019BF783CE9877749516CFC76969925A09451A277893CE3280AD7DACE' +
    'D8C66615E074D4716103CA5C10757027995F8F98ACD141AFDB4CB1531089525BAA01B906BA4ABD0D' +
    '6C186ADC3B96691965A883034C53E50268BF8A6CD954115FF64D6C9630CEF8288E6395C2EA011CBB' +
    '3170F79DFA0759F1D1CDC475059FFCC375DBA3AD9B064230833060C803182957F80EC9772DC235A5' +
    '8B16C38A365176F3147C5B524AA13E2296A36F2964C1638FD8F7570CC572D7D69E2B81985C580A9B' +
    '8BBFF08A3FCC757FEE6040FDEC7852A32A05D51D8FF338C0730242CA6DB2A4BAEE99C29749A33E13' +
    '96FC680B6A7589089477DCF44FEA18AA2C925900203200AD14585239F0727B4387530BF68F7E077E' +
    '62A108581C8127FB1C5DBB5E04582E746894D2516995434D81380FFD374A0641776B218E38C97862' +
    '2A5BE4EC6072E38A1F0289B060C2DEC91E7F763702F17803A58F094250D2E1670C7B90F0778365E8' +
    'BFEB2B3556B168FE55B854089E52E0A2B8D832DA57AE355091C876AE67B367CA866A1C3597AD4CCF' +
    '4E5B2FCE0788593141D2C0CB6B3C9B80D3445F992EBFE98F26DA68D03669503A0EC696B430B2217E' +
    'D9C4B6CB22BB3A91D6AA6C1A2A3F3615BF67C5B420EABE7159363F525E5AF9049D4C0C9ED197DD7B' +
    '6F8B4C2F6604C8CEA75771A3AEEC0C0E8D1C8A6AE94FC8485206217CF785677E34C458795D630340' +
    '3F7AA427763C5ADC4DFFC8C2FD422870B876FAAC6D334931F0D760ACF4092A0727F1935855B20698' +
    'B5C8B2879576A25D9A9B93862079DC02CC30C731FDB7021C9631C43230A6FC6DDA1956F3EA01DCAA' +
    '64354060CD4C67E1149915A423FCE3166272B2E3B71E3BB509AE978D88859B55E50D338B703E1B1D' +
    '77D44212E00D510695974A07B8250053B7129889769B4991B5083E05D0054FC4CEA18C0A42912985' +
    '78BD53B11D174C6FAEBB6E5919B2E7BB7A4CA9C94E45ACC1674D5D994D2AF81CAAC6A153334A1B77' +
    '0937F14DC2696721665F87751B608C921B985215F40EB28BA632533119618A97D0766FD780731857' +
    '1EF8305175724E54CA261A03B8243B7D1087F70A39A7FCBB0B223CBD11AEFEA9BBA4833FBED98ED4' +
    '8120B3CB507C2C272E7A6EE6C92EE5401B65345182C31901F1C0A82743C2512A392C3B6BB73FADD8' +
    '9131FBB65610C9407C62860B7EF894CD353C02E347424185C27A06209914444FBA9C751CC27D5052' +
    '7531B282A6C503594571CA89759C345632AD2AC24DAE862EE091C4F421194ACCC9F45792B69C1221' +
    '5B878A4ACE975B295C672288472A2B5CB3865C787F7A3DB60608E648A11E6BBCA38456F93CB4CE2C' +
    '31FF1B660A096832E518B6B7032C20ADB3830E982430EFE661C3E50802487CEEDB0FD8A004A72634' +
    '83F1B41227B96C207EB1B2131082CD85244310AB6D09D4A73D436BF3E8A275ABBEAD804968F9A5AD' +
    'F2264F897463D71FB09A797EA500B671C0062C04B911606293B8C7BA6186C2A2DDF074C2A6F36D35' +
    '796EF0FF1CF8E47273048F82A8F00AA9040D87CB0586D968';
  S_DK =
    '47D60DEDCB1A694A5F55A066C4E61A4305346BFA8F03BA2FC7E01456471A9C1C02B2F471A3DB9D10' +
    '056FC048652C545621000DB032386D2ACDFD81119F781CA0D14DCFA78C700B700BBCC55D3A3951BB' +
    '82FC662F1B26C9A4F360EE266CA49585E696625D9787CEEA807E8506B4A38302969298BCB15A4219' +
    '460C653A965370D14CD7874E711791CE719880F4A61E086A560457B1B002A9B739292A4459CB415F' +
    '76A4597C2325599BCF18CB44E7CF3F53091054CD3024543494ADE226BE9D4541930AB14CB8226886' +
    '3B9B569EA528BAF3601FAB2831F1C29B4C4A712699B144CCBC63C90FB1E1779046CBA85AB61E23A9' +
    '7E57477FD84F022B20839B2F967B761CF3916BCB2D088850BFE2BD3CC40DFC5170CD7086F1D61380' +
    'F338EFA2BEF1950315635BAB527745790495692664D1A55B3912DCF0AC797291F98249EE91660FDC' +
    '8A67407F47C3B39B0857F1E1A2C0082D3716C0F7D0AD35229A795A3EE41A130BE53BE1173A94D3BC' +
    '712B9F4CD86D5594A2C87A25642311ED45230B1839DB7050B8CC7D2DE99D90E32D5C2C9DE82BC56F' +
    'A850886C6B549CA86E480E3373092B5ACB628C497DE3A789167DE055740B9C3960A52E1C54A99CB5' +
    'B4221B7B1B90BA04E0949B39735F8956C30748C0CA7D6641CA97B87C30E7BF57E02166175E411A04' +
    '6645CF4B1C63D7C383A46A957A74593BA694E2446462231FACCA0BFA3B19AA469022C89B48D436BF' +
    '4C4411549115D44857B27718754A2E7A025A693C858A594D52AE839465188693D8D7A449F3747CA8' +
    '8B0429795331AC53B565A0BC44B13B7D73FAC5AF606BBBEB73BDA8429136C3EB77928A3879507A62' +
    '12216E6060095E2294F2C6B708883877D73970D306072B411ACA1A51EC1B64DA86F71098C46C4C29' +
    '08C9AAB11B4D5BAFF7B205F1A1772454450F2A84723B87BB4A6B7CD5B07300A2EFF9A2A4BABE3232' +
    '0B7D268EBCCA1C42BBCEB9964950B71F507A2070F73BEFE482F4BB4750D2C6868AB1A42C6BD731A8' +
    '5EC036329B122E937B2786961B76060AA0961971C90887B468A23A3CE8C21E2AC2B6C6613F0539AB' +
    '48946E6920211894CE970E9F671FC3105BFC7A7655A667E5A6775EA970BDD37D289489090A292E8C' +
    'BB72D78A86F697752CAF79AB6840684356CB2E5D632C64B986A4174B05CC3DAED01F4CD286413CA8' +
    '486AC310C61797815797433C38CA6352504237486C11CB383AC3C1A957586EF69BCF52A091F10884' +
    'A515474BC423C11630446E1F7B98D7906FC4295F00473094A01B325A246875A90704B44349A460F6' +
    '663BD6BE2B954CEA2794DC802AB6160400634726D07B40157A63DBAAC7EC5D371502E51C0A99461B' +
    '8FD23FE6492684E76466C661C886BDCC16ABACB56E1DE361E6F4864BE32EC3B58558444C612B9135' +
    '1A7AAB3C415D475CF2A86EDAA91BB9B5B7FF8184824B2D9D36509E00348366483B0080883786F235' +
    '3CDF459100D9AA21B279189C4652F24071C9BBAA5A697B03756EA3AB084407E41CCD2B66B044D830' +
    '70D49AFD0716FC79AA0820BAD5CCCF26379DF4432E87C4A8C5B85F181222333BC7D7E5CE35252CE4' +
    '2CB518BBBFF00BA03959989370974388B7F0B64D6697B631145E9C5036D8174362387A619993B5D9' +
    '567CE83676994DC54C46D6E380A8944E0DC60DD6312460E698CFA6AEC0AB440C3A3D93F21F65A05B' +
    '42933CA538108BE5235F8A69B505CC2ED84CC0606EEC5B1449BC5F12048074F05985429AFB6C9FBB' +
    '7140C28019BF783CE9877749516CFC76969925A09451A277893CE3280AD7DACED8C66615E074D471' +
    '6103CA5C10757027995F8F98ACD141AFDB4CB1531089525BAA01B906BA4ABD0D6C186ADC3B966919' +
    '65A883034C53E50268BF8A6CD954115FF64D6C9630CEF8288E6395C2EA011CBB3170F79DFA0759F1' +
    'D1CDC475059FFCC375DBA3AD9B064230833060C803182957F80EC9772DC235A58B16C38A365176F3' +
    '147C5B524AA13E2296A36F2964C1638FD8F7570CC572D7D69E2B81985C580A9B8BBFF08A3FCC757F' +
    'EE6040FDEC7852A32A05D51D8FF338C0730242CA6DB2A4BAEE99C29749A33E1396FC680B6A758908' +
    '9477DCF44FEA18AA2C925900203200AD14585239F0727B4387530BF68F7E077E62A108581C8127FB' +
    '1C5DBB5E04582E746894D2516995434D81380FFD374A0641776B218E38C978622A5BE4EC6072E38A' +
    '1F0289B060C2DEC91E7F763702F17803A58F094250D2E1670C7B90F0778365E8BFEB2B3556B168FE' +
    '55B854089E52E0A2B8D832DA57AE355091C876AE67B367CA866A1C3597AD4CCF4E5B2FCE07885931' +
    '41D2C0CB6B3C9B80D3445F992EBFE98F26DA68D03669503A0EC696B430B2217ED9C4B6CB22BB3A91' +
    'D6AA6C1A2A3F3615BF67C5B420EABE7159363F525E5AF9049D4C0C9ED197DD7B6F8B4C2F6604C8CE' +
    'A75771A3AEEC0C0E8D1C8A6AE94FC8485206217CF785677E34C458795D6303403F7AA427763C5ADC' +
    '4DFFC8C2FD422870B876FAAC6D334931F0D760ACF4092A0727F1935855B20698B5C8B2879576A25D' +
    '9A9B93862079DC02CC30C731FDB7021C9631C43230A6FC6DDA1956F3EA01DCAA64354060CD4C67E1' +
    '149915A423FCE3166272B2E3B71E3BB509AE978D88859B55E50D338B703E1B1D77D44212E00D5106' +
    '95974A07B8250053B7129889769B4991B5083E05D0054FC4CEA18C0A4291298578BD53B11D174C6F' +
    'AEBB6E5919B2E7BB7A4CA9C94E45ACC1674D5D994D2AF81CAAC6A153334A1B770937F14DC2696721' +
    '665F87751B608C921B985215F40EB28BA632533119618A97D0766FD7807318571EF8305175724E54' +
    'CA261A03B8243B7D1087F70A39A7FCBB0B223CBD11AEFEA9BBA4833FBED98ED48120B3CB507C2C27' +
    '2E7A6EE6C92EE5401B65345182C31901F1C0A82743C2512A392C3B6BB73FADD89131FBB65610C940' +
    '7C62860B7EF894CD353C02E347424185C27A06209914444FBA9C751CC27D50527531B282A6C50359' +
    '4571CA89759C345632AD2AC24DAE862EE091C4F421194ACCC9F45792B69C12215B878A4ACE975B29' +
    '5C672288472A2B5CB3865C787F7A3DB60608E648A11E6BBCA38456F93CB4CE2C31FF1B660A096832' +
    'E518B6B7032C20ADB3830E982430EFE661C3E50802487CEEDB0FD8A004A7263483F1B41227B96C20' +
    '7EB1B2131082CD85244310AB6D09D4A73D436BF3E8A275ABBEAD804968F9A5ADF2264F897463D71F' +
    'B09A797EA500B671C0062C04B911606293B8C7BA6186C2A2DDF074C2A6F36D35796EF0FF1CF8E472' +
    '73048F82A8F00AA9040D87CB0586D96885B34A8FF209DDD6CAF616A2883C143BFDF14DB231E98CB1' +
    '19C2656D1175BD0FE1A8F78CBF6F01A3878C8B0C421B83005D834A668E52F982855D5C943A1D93EE';
  S_CT =
    'FD8CF594954FE5FC75D774B4390EC1678B676770C602B7C128070BD331D260AB21D1F99C726729B6' +
    'FBF8D699EA133B1665F63DA83F737E938A41B865339D10ECD26940BE601870478E0D9E2FC2BA385B' +
    'CCC0A786318D9600B5E8F269DEEAFCFCF61D5138FD905ACCB595D239F7643263DF754C55FECAB8F0' +
    '7DA82DFBF590EB6E175F6283879BF589B052B9CA2130B7025C47931A8AFDEF26B3843F6D2C701A7C' +
    '3075EAF545D07E6280F261CE1EE3EB967F889651430C89693B6853F8B9FC32D758C6947ED409BA4F' +
    'C00238E490059995A81F7013133C6BF4D508D30E3CEA08C0D84ADC2C1A465FF538405201B31CEEA7' +
    '1CD1568B98B17F4856571150674745C5858D770EA4D91E40148DD056C0D8C938B380FACCCB2743BB' +
    '2D728C11F665717CA0C271316496331F8A2583FB1C2B3E53D668C3F485993F14DDF234B7AEFDC95F' +
    'AFC99CAF0F44845E5D7F727B70036F5909C54B56B3CD842C89907408E600C14C7C00AC09540AEE9D' +
    '25619CABBC3178184F2E9CDF0A3892FFDAA75C7EEB4F28CC9EA0AA8A1F63E4A9F9373075214C7B51' +
    'D606B79F12E95D7645CD2D0158DF5D70C409FCB01ABDAD1FE33ADA0065F3DE67D49BFE9919C74B89' +
    '41953B2864AC797A2CC5B2FD887B1C0C9FF1C9203D230EDB5A59DEF96F8BE673D1955422C7C16FB7' +
    'EADB3DB33748D0413E94A5F85D9A020733629F93574D8687D78AA1D97A10D9B1392166B1E0D0B6F2' +
    '35CB8EB8A5CA56981511B2805D7728589F75E5065E40B911CC7253B67012B6370222BC1EEA263424' +
    'C307F3F4BDE39EFF597F31787164545EBD1387BF88A717F1B5194947A5CA44BDE7A176D50D1EA7E3' +
    '72D95737606743A714E64CF4C1ABF8A2DF7BDC9A15A8925C502E2DEAC0CB2CF127E25DD002D4D367' +
    '85A3042E8712943CF3E9441DAFF33855A8F2E1DBAFB723FC789A174ACAA808E574020B252C3E99B0' +
    'B170FDF0C68AD6039D593B3B3B96E408AF85E7CBE60B275F4C0E3E1C0E793DCBEEE8B8A8BD3BAB92' +
    'B263C0F6AD2971F462537ED9ABC092CE6FE617B33BF731EDD9DF66D6E626E5F173CBE0B246B3A0D9' +
    '77B28CAB98B5EE4857C7E620525F51D8F6A83668768CFD68E5B8EC2E0A1A17B4BA5792CBFCFECF09' +
    '62DCFEE00E220B438CD75F672B2BE7E17EA85D4070E12D2EA716D9710E52F9025324BAE8A76DA34D' +
    'BF531629411CACDD7D5CC982FBF2BD6D5D481CE758A431C6908092B77750DDE6859F1E09620B5358' +
    '57B082E72416F39B7169D57C1151593976F5782609B5CF4BDA74D9C8654E3AA8C420540ADC732593' +
    'AAAAAFFDDCE25CD9B554B9EA31900A93E06C5CC761F592074425083A4C7746D05B138E234DF6AB7F' +
    'E0B24A900E03DE7AC166DBEF702E5920D023DC828655609C9CC0EFA2297BB26C439889B099B3C764' +
    '2323719F6B28E8C7E8D92FC92B69772ABBE5DCC47EF31C37BFCA8EED7BDEF7941A986576001009DB' +
    'EA7213A096D1A51A3F27DDEB653160837914AF4F960009E541ABD0A669DCC54FD6261D4D3F3371E8' +
    'BB3B59C8E38EF4EE';
var
  M: TCnMLKEM;
  EK, DK, SK, CT: TBytes;
begin
  M := TCnMLKEM.Create(cmkt768);

  try
    EK := HexToBytes(S_EK);
    DK := HexToBytes(S_DK);

    M.MLKEMEncaps(EK, HexToBytes('DEAE13A40F060C5A154826D5FDAC7C271E2A7C8BD3FCC211027F4B7C2E67C0FC'), SK, CT);
    Result := (BytesToHex(SK) = 'D5722BD3F338D3A4AAACBDE95E2768BFDCA819F719BADD1C8FAACF859FE65EA0') and
      (BytesToHex(CT) = S_CT);

    if not Result then Exit;

    SK := M.MLKEMDecaps(DK, CT);
    Result := BytesToHex(SK) = 'D5722BD3F338D3A4AAACBDE95E2768BFDCA819F719BADD1C8FAACF859FE65EA0';
  finally
    M.Free;
  end;
end;

function TestMLKEM1024KeyGen: Boolean;
const
  S_EK =
    '5080C6154706AD0819FC352BEB0747F728B96C8AC02E599764CC43B646BB978069E29B4527860F2A' +
    '746071C392B2284F42E3582341587C320997F06E07E07F9BC3932AAAC4149768BA26A086878A8049' +
    '79AEBB84C747416B4B17C020975B459432640B137A74388B068DAA2F22D2C19DD259646887AF9933' +
    '4E37CB76399C4A83973A3A08BD37CA197A474B1902A9BBCFD86B65EA138FEF199D5649865C8590EB' +
    '866D1F5792D316BC84750314075DB96977928131B868C3046202051A7F0C05AAF74C5535B401D75A' +
    '243AC9596574CDAADCC9FDDBC4B77C3EC93630EBF4635E34B50E7371AAE50E0085BB67C055FACA39' +
    '4DCA9E13F74048170520919A96911C76D19F77564B4B9C361C706A528741D463C468342171A48EBA' +
    'FAC52FF36EA6544A29B1762AFA6114995E6A223535A70B2F6B3D88772FBDC512EBC1A790D8995E71' +
    '0C68C9BA7F1C6DE25BC9258A9368FA41C0150B4C629C0F738B3AE056E8C678CDC2B623F3BA450C5D' +
    'C4DBA6861102BFF93C9CE0909BF21CE5E6B0D4A095088525B15CAFE3630CAA434995291246A4370D' +
    '1220CCF717CF7BBA1550C36A55A4319B080A6B28BE784A3CF1AD1AB3CC85472ED4BA88DFC818FA46' +
    '5C7A4B5B1D1A6B6A1A90EDC74E63891B482A3AA1A238997C845C0B8B5974C68668B36D0187F19955' +
    'AEBC873A9685F4A5380B597821180AA26748A466989F7AA4A8F982252CB9BBF6A79EC16E5A671185' +
    'D7AD97B91E0D161E1C6343C80801302053A295B6DF78531C3753F464594F2142FBE4CAB6D44C5B43' +
    '1E3318CDFD1B80DD2C6A0E93719B0813E299860A04AFC6715EA088A8AF15A5382B3CDB1BC73F923B' +
    '2273B6DB9425E1290036F966BAE631A94C81C49C90482B3BDEA102361A8C33AB29443509CDEA3FC8' +
    'A358FE8563365AB5DF442F95361D4CCCB9802046A3042F139346F891B0C8A348078508CF8985A521' +
    'B062266AA2F10E6F8336BBE36580BCB0E1C690CE15521A7382CC66A39E1A922271A6B4F25690F379' +
    '81D610B8FB63E1A3A19B935DD2215B1A42AC8EC22EFE0940666A9F1CF89168426E0BF57D649B4738' +
    '2405949AA331D401015070268C484DEB1BA629CAEEB30A33A831F4909C5951C46C49361FB20AFEA0' +
    '5FBC24691B87738601723A730106AB2EDC1250838964BBC435478B20707CBEEF1106A81CCFB819A3' +
    'CBBC401030019D1C1CD0C0824E0224B058567860B3BC7382876948E13696D9391993C319D6F135AE' +
    '8366E36C16D9028E320B39EA94A60E1573886C9B669896EE070F84A59C9F51298552C82C78C9B882' +
    '38D064CE1686A55D9B7A39E2398AB0235F2A389C640BCE99C69F7603E2A6222148007D29AE12C554' +
    'C65103494B40E02613907C467A2696114632BF1A67B2B708E0349082F84859921DBD83CB22341003' +
    '2A20F6370381B28692FA2AFF40C3BDD133019894BE117330CC8A61418C890B821D82011D6A487899' +
    '49FC851CE5570411D576A4D794571B2BD0756CBB35A5C21655C2C7CBAB57A6BE7B675DD4B294E5BD' +
    '8705AFEE8C8BA6BB3FBE1B4D6ED6002E8B3944F4A7ACCC6651D16E5EB957EE43CAEA48BC7C3B8627' +
    '96A709C95B953CC095306E275119C7F23F85E79EA37B28F6579E4BC99A9B82B86EFB190989290609' +
    '8B2196A9157437A6EC62F0E5CFC29394F1D43E05734FFCC262D2503527937896E9BBC394123C1BB0' +
    '8E5514BCC4AF1C49A6CC408B3BC93502A0AAF6A67E8F2616E0E389BC8B7B646164D68156E0D8627D' +
    'DABC97759A0C9C87728814DABB959D39A239A376D0232BFADB8DD1512389C5051E1920A7E43E22B5' +
    'B00B8974D7B690CEBA1A729366DB0508E8E660559351DD929BB0734EEAABAF42744CA1838F83F700' +
    '0A0BD057819D51EC601179880BB9C47C01A5C002B31FB3B47BDB207644A75FFC713DEC4A1EE1A743' +
    'C628B2DA48A369750A80A8571757E12546241A5408D63F53E86BA3F403FB93C0D9749F78E1B4D180' +
    '2DCBAA8DD6AB3446FB615EF2C00646B605BA117C666F19DBA196036329767293561D5FC0C648C280' +
    'E6E00242452A3702CC2EE9C94CE269E27A4FF8464EC2D50957AB7CF6829B3F6C9598379985D66989' +
    '984834164AFA794D0C6A126B774F97107EDCEAB95347BF89234B2446B2CD4232A39A95E0932E07A1' +
    'BE1646B789527661442563155853218DD9B942BD7436B9153F1E8971AEECF473E37E5AA32806ED14' +
    'AC214E005F0BDDF3';
  S_DK =
    'C37255A4218100D51B10510E1DF6AD6E3AAB6285046EC40D66174EA1C1A82B210D08AC71F1132666' +
    '518B296A55720BC712202A694ACCB2E5BA19F831A12730F8D00752A71279A7C588E086F802159457' +
    '19671840037C70D3179B93988E424086D7E322E87C24987C0325DC70A7B72D3762948A749E5EAB95' +
    '3A5C383415BFA9B72B3CBA4ADA73AF7D39AE4CF85088F4BC2B91ABFF19B0539A0DE80288E2D33D3D' +
    '9486C4FB1E794953863B90287A0400E5B8841B836BE0CDB06087751AB48F011F1AC27C116093FF95' +
    '92F47237B0BB1BB75489A3058C46EAA4F9976967D985C604B8D878B9150648F8294E8590CD134112' +
    '051CB9D4E2BABB31218B07CD24520EF1C1036DA786C5671DBED54AC60A3CC7591E5E34029EC0B505' +
    '5A68B3C34158015ABF5B6D479504E3E62ECC9868DF0578BF0677CC22B037BA50088C0374561A90B0' +
    '8DC93A621EC668F8F1AC391C845740453020BDD180A653B6C67057CDFB33762226254CDC054AE92B' +
    '3F388FF8715EA5913D05405824A80DAE89138F316845C8484AD21F8400336EE449CFEB4C42662676' +
    'C279C9572650EA71B7029729D254CF230A75D52690AB0271275D02949DE7951F3026BC9FC947DC29' +
    '03E8B79201235D1DF22D86B60099B54EAE061584F959C2F977E30CA6A1554DE357349EF9B9F52B4D' +
    '4B49CA1C76522AE26432B145B9D1AC8FF38EE25C7A40340889DCCAF977BF067669210BC2C2E07D5E' +
    '13A1248CBEF7177AA5B3A6CE2594ED4013A9CC0DAA994436B0606195C8D31C3BEE363EE91BBA75A3' +
    'CF082C2D68E8B21924092592664ED599A4851573147FBBC31CD599BF4D14C8FD25439E73894E022C' +
    '84D266A8F93720A8BF883847B0B162EA6BA4D069467717CE9B1550ABB783755715F5F59CC7FACBC7' +
    '69020A738B9DA68F96B594EBBA3E01211ECFB24CCF174FD1CB5F92D715D7D573A80A4544A239A673' +
    '68CA805ABDDCA798FC51E0730A8912AFCA9C90EFF6A0A5925223C795EA010327E20C903A4715F595' +
    '3AA6A776812673152D41B66AA99960F5666FB6981F55035DFD6423F758343FD7A3CFA80205C69E8F' +
    '49670B617FD2D65332D57C42B0918B097C3EE76D257C8966A9BFCB547C81987A7C751D74372435C6' +
    '8927D57C2376A5712A815B679468729E421992EA989BD3556741E15721F55535C74F6F23BFDCD09B' +
    '64B78ABF9BB0ACCA3F53E29BD5A8A9E0295FFC2A80FE2B2BDA352F8E805970E43AB0D51D07B9A467' +
    '3900CB1678D7C2BD5FE4086459738516BC37A66D91950BE2DA965D9BCD71B9342002C74D6A70B30A' +
    '25D0FC1949A64106F018451A7D38837DAEC8A6FC6688B28A820F385FC3FA9932033338282BEF2707' +
    'EAC64427B70A9E39B970D8C0C39374D381624F2B058EDB6FA31CB22A632FE939880CAB94AC0590E9' +
    '90B99AC51E573049ED1CBEC78A5CFCDA33BAD55489CAB062A7ADF517140FC40947F333FC893DA1F4' +
    '1C403C4231977AAAF4A1631CC78D626ECB979C981B17B5FA78A85829C679B361E270F4EC9010BC14' +
    '3E0A596E98B0E1784D314B119C330A9C9593C2885E5F104E54059E60A123DEB636B1F1078A0619F9' +
    '5A9027BABFF6148FA3A7910DDC525248C556882214F90610397C50A5837146C3F5C5619ED87EE772' +
    '6B07F9B138681C64CAC2EBC32B4FCBA3593C0716F1624A38A454669BAC50AEDD76139F0468714A10' +
    '2D3248664397D1B17A4BBAC8000C28ECA20908B32D6A501552FCC805E655BE05C004134AB2D27C22' +
    '25424499B98DF63773547351078BBE003105F60502306BD3A190DCE9BC83CA41321CB41E01C32E47' +
    '36ED20C7F0BBCEBEA01B364B58D758827878A8265446F021C014F5493EC4535438139FE9A0AD702F' +
    'F7C1A61537B1EF85707AA5A96B071AF3629B5B86380625A8E5A93B7BCB937CF4A1CCD1A2DF4B4B56' +
    '99672C1013708A8F333B51F7E23125DA51FBB47ACE42275A36BFE2B70B381B0B8FD19DCCE7B9A33C' +
    '9097827899483893C30C40B16799651092E8C3E8C051AF8303CA931F33998A097C58528BC410137D' +
    'A0797C9B05BA4F954125435E9DB766A9E780CD4C316B823567E70B2DE2864FE7480DC6597984BBE8' +
    '9458C4B0198508BA5538385C1B10E746B9AB26C857616FAEC33BE89CA551A8CACF43236CE1CEB13B' +
    '1CE18BB2E15A0504B92C9708CFA093185080C6154706AD0819FC352BEB0747F728B96C8AC02E5997' +
    '64CC43B646BB978069E29B4527860F2A746071C392B2284F42E3582341587C320997F06E07E07F9B' +
    'C3932AAAC4149768BA26A086878A804979AEBB84C747416B4B17C020975B459432640B137A74388B' +
    '068DAA2F22D2C19DD259646887AF99334E37CB76399C4A83973A3A08BD37CA197A474B1902A9BBCF' +
    'D86B65EA138FEF199D5649865C8590EB866D1F5792D316BC84750314075DB96977928131B868C304' +
    '6202051A7F0C05AAF74C5535B401D75A243AC9596574CDAADCC9FDDBC4B77C3EC93630EBF4635E34' +
    'B50E7371AAE50E0085BB67C055FACA394DCA9E13F74048170520919A96911C76D19F77564B4B9C36' +
    '1C706A528741D463C468342171A48EBAFAC52FF36EA6544A29B1762AFA6114995E6A223535A70B2F' +
    '6B3D88772FBDC512EBC1A790D8995E710C68C9BA7F1C6DE25BC9258A9368FA41C0150B4C629C0F73' +
    '8B3AE056E8C678CDC2B623F3BA450C5DC4DBA6861102BFF93C9CE0909BF21CE5E6B0D4A095088525' +
    'B15CAFE3630CAA434995291246A4370D1220CCF717CF7BBA1550C36A55A4319B080A6B28BE784A3C' +
    'F1AD1AB3CC85472ED4BA88DFC818FA465C7A4B5B1D1A6B6A1A90EDC74E63891B482A3AA1A238997C' +
    '845C0B8B5974C68668B36D0187F19955AEBC873A9685F4A5380B597821180AA26748A466989F7AA4' +
    'A8F982252CB9BBF6A79EC16E5A671185D7AD97B91E0D161E1C6343C80801302053A295B6DF78531C' +
    '3753F464594F2142FBE4CAB6D44C5B431E3318CDFD1B80DD2C6A0E93719B0813E299860A04AFC671' +
    '5EA088A8AF15A5382B3CDB1BC73F923B2273B6DB9425E1290036F966BAE631A94C81C49C90482B3B' +
    'DEA102361A8C33AB29443509CDEA3FC8A358FE8563365AB5DF442F95361D4CCCB9802046A3042F13' +
    '9346F891B0C8A348078508CF8985A521B062266AA2F10E6F8336BBE36580BCB0E1C690CE15521A73' +
    '82CC66A39E1A922271A6B4F25690F37981D610B8FB63E1A3A19B935DD2215B1A42AC8EC22EFE0940' +
    '666A9F1CF89168426E0BF57D649B47382405949AA331D401015070268C484DEB1BA629CAEEB30A33' +
    'A831F4909C5951C46C49361FB20AFEA05FBC24691B87738601723A730106AB2EDC1250838964BBC4' +
    '35478B20707CBEEF1106A81CCFB819A3CBBC401030019D1C1CD0C0824E0224B058567860B3BC7382' +
    '876948E13696D9391993C319D6F135AE8366E36C16D9028E320B39EA94A60E1573886C9B669896EE' +
    '070F84A59C9F51298552C82C78C9B88238D064CE1686A55D9B7A39E2398AB0235F2A389C640BCE99' +
    'C69F7603E2A6222148007D29AE12C554C65103494B40E02613907C467A2696114632BF1A67B2B708' +
    'E0349082F84859921DBD83CB223410032A20F6370381B28692FA2AFF40C3BDD133019894BE117330' +
    'CC8A61418C890B821D82011D6A48789949FC851CE5570411D576A4D794571B2BD0756CBB35A5C216' +
    '55C2C7CBAB57A6BE7B675DD4B294E5BD8705AFEE8C8BA6BB3FBE1B4D6ED6002E8B3944F4A7ACCC66' +
    '51D16E5EB957EE43CAEA48BC7C3B862796A709C95B953CC095306E275119C7F23F85E79EA37B28F6' +
    '579E4BC99A9B82B86EFB1909892906098B2196A9157437A6EC62F0E5CFC29394F1D43E05734FFCC2' +
    '62D2503527937896E9BBC394123C1BB08E5514BCC4AF1C49A6CC408B3BC93502A0AAF6A67E8F2616' +
    'E0E389BC8B7B646164D68156E0D8627DDABC97759A0C9C87728814DABB959D39A239A376D0232BFA' +
    'DB8DD1512389C5051E1920A7E43E22B5B00B8974D7B690CEBA1A729366DB0508E8E660559351DD92' +
    '9BB0734EEAABAF42744CA1838F83F7000A0BD057819D51EC601179880BB9C47C01A5C002B31FB3B4' +
    '7BDB207644A75FFC713DEC4A1EE1A743C628B2DA48A369750A80A8571757E12546241A5408D63F53' +
    'E86BA3F403FB93C0D9749F78E1B4D1802DCBAA8DD6AB3446FB615EF2C00646B605BA117C666F19DB' +
    'A196036329767293561D5FC0C648C280E6E00242452A3702CC2EE9C94CE269E27A4FF8464EC2D509' +
    '57AB7CF6829B3F6C9598379985D66989984834164AFA794D0C6A126B774F97107EDCEAB95347BF89' +
    '234B2446B2CD4232A39A95E0932E07A1BE1646B789527661442563155853218DD9B942BD7436B915' +
    '3F1E8971AEECF473E37E5AA32806ED14AC214E005F0BDDF3284F5922E1FADB421CC725F649A466A7' +
    'AA9D04C09F8C42EAF6CB63A3632150032B2BC532E1B9D30739CA9C680296C7654464455ED2C95A77' +
    '7C3BE1E49EE746F4';
var
  M: TCnMLKEM;
  EK, DK: TBytes;
begin
  M := TCnMLKEM.Create(cmkt1024);
  M.GenerateKeys(EK, DK, 'B24959281197AD7C5834852DD145A3A5F80CF7CBF877CB5CF11E583BF4676162',
    '2B2BC532E1B9D30739CA9C680296C7654464455ED2C95A777C3BE1E49EE746F4');
  Result := (BytesToHex(EK) = S_EK) and (BytesToHex(DK) = S_DK);
  M.Free;
end;

function TestMLKEM1024KeyEncapDecap: Boolean;
const
  S_EK =
    '007363361C85D85753779AC217569EDFF54644109BE7F252D3A59B405074A7724FDB408736B5B459' +
    'E7151B05057CACB5FC93A59078A0D4985327973A26AB058BC41CDEB857311C301EFB0653817A3ED8' +
    'A48E29865DFC22707760D31117F5D14070552295F361B7570600839AB4C23B4BFB32FF7B0D005912' +
    '8012A11552A2C73633909B68D76665125972B66BB73F6053DA0C107E989F50A47243F934A5313CCE' +
    '4978614794AE956293E165D22198A3220826367299524058591841FC209123A36E492556C20039A0' +
    '9604834E90732D341C7CF7792557D3C05DCA2C5C66139C1A5192B261246C3347A95AEBE227DE2747' +
    'B2FC3C36508BB82248E604CE4D13570F452DC36A285AC465D9B9791F4A52DC57013E449F9F098AFD' +
    'AB740DE4B8676A5D3D238CD154ADE2B98EEE09B1BF5C1FBEE70D495B5A08D5A7744248C90A0A5F9B' +
    '87602C3A6E6C5E0044C3B22632A2C32191CA0DA0ACCD91B58858EA239C70353E8C576E383492204A' +
    'D3BB141AABBA2FA199E2EC94A9B87E24B424A5F9CFC226741E00AC0BF0A648DAAA39A7571F2213F8' +
    '9A93BE88AF6DB80E7459CA754A0A9CD0912DD50FE3AAC42D14416BA59EBFABABE0057A984849DDFB' +
    '162C266C7544B42C43AA09930D7E76873FE879433B3EEC0098FA5B4319D471184AB94C8474E508A0' +
    '6032BFC6612A8784199B483741C0B929195F00F395CC79BBE058736439830FCC96BC5A2FD8A878C8' +
    'B07355C02B46BB6833741187146F30525A7A149E52F3C87C86C489655589D06767EBCEB113453883' +
    '9DF2F81CF8977201296B96709D2E57AAF8215A07410C925854E91567BD78A09BD815E6F954FC932C' +
    '444493F2CB801512C5AEDC8ACE9B1F0A02ADD6580885038FB3519F85A4C8B473C2DBBC5F491358D9' +
    '07ADBE96958A1839BF371428BB59DA13A1968039580A30F51C6846B03066C624A40C0236529C5964' +
    '3AE741C0FF5C5EA4B3B527C53DBE26883ED84F3FF09F47EB5DCF2A2AFA1BB598964E89CCABDAB75E' +
    '5AA63AD9D42CBF56ABBD45798F1232C23462D2FB86FF066C28828FC1012941333552D87D8ACAB64E' +
    '82C1E3231C4834BC0D2650D4A96FF2C27D83D88FDF457B26B77895469AC1A0A978C763DE93086136' +
    'B1921AAB994769FB0697937420D1A398431C21E9113796612F7A2A893DFAA09AD12F0DAC553F09C8' +
    '7C25A09DCC2BDEDBCBCC61508E26A0DC1099C2F530BC8756D8CC7C6F134CE010381F16AE4F6A9E7E' +
    '62907BF289C0B16882D71EEBE31BBDE1A45E0C69578548E3D54E4A362E2C3A8E3FD5AF20115CFEB0' +
    '4258FC728C4BCC16D4C655CB7D0A783EC6DAAD3350BBAE53009C608784742303F95599AB19197236' +
    '691056E9687B0BC623B49A782E855DA6C8AF5695B2D1B60560B2C12C0939F6E745F13895E3C672AA' +
    '984183BA7B47AB70B26603FD84212602372DD49A99342D48E0B573113FD2CCBA3EC57C79770FB223' +
    '98533229B60B93C72397C1D1B34AC03E2BDA48836B49AE42C5271786A63B9F542B9E64D106ECAC63' +
    '740CB2E18BBC141B764AD5A6EE004AB80187A3904FD8932497AB46C930293545A19750B6FC6C026E' +
    '8C3B6B767795EB41D6800ADD791DB3871B6DE85BC39B5DDAFB09BD459F6E3C3229A67A96F288A7B9' +
    '82B32630C9009009C2A0C6B2B1EAD47A033CABCDD687979CCDB2160CB3653DB6B9B735317AEED78F' +
    'CBB13E7BD38D0535B8D837653D91AFC70716BDC669333C363E4303A818B2FDC66A1672708CF96768' +
    '32254C043535334408222A1232A6D92459E25004148322080AAD50B9BCD76B24D849C05E6825BA49' +
    'B135C136EF8B4552A29DE0184BB9E6B50430892C0A0854F57ADC017E6844893F75025941123CCC83' +
    'E4030DE481484EC0A1A9F896DD344A6C6BBC859B853AB47A5BB51E97D04E391312A14268B66B6DEA' +
    'D8A9077318DC8844F3F5B9D17A759E864F6209D0A95C3C345183EB5025AA6476210710FF5777D459' +
    '24D302BF298A7FB3A54A402278E527AD946A6D745B5F58B886E3F48AC9EABB92CB840BF5CF907C6B' +
    'A4434299C919659B6E4711725B47915720C7AD4A8DD5F16AF653B742F6199AD94E2127748CE5BAB6' +
    '85CD23CC5156E8BE17F8786BF341A4D1A1A6D99CE9894A6BE576C7DB7BCDFA6E5B95ACE6DB6A38BC' +
    '51EEDBAE49D61ADBE43B63356C4B260481360B285A4B26501FC8B6E5709E5997EEB2359382FE949A' +
    'D53490A5B2ED1093';
  S_DK =
    'BBE82278E97C0F280BCBBB0C0ED87E235787285293D5E6AEB271384D34B2A01C71973ABBB1270EE4' +
    'AB69C8936C09A9062A357050CB33F712B8707561571690172230E504CE754A444FE3329B22B30D92' +
    '2BEC9197A84683230BB7C2333B00645CB8E478E43B34199126BA336F5C813AF8460F7C975A0CC6C2' +
    'ACBB06C99326CEEC73FD5755D38B40AFEB87D78A30D470859451B28C2495AB8A49921A91DDF19EB5' +
    '8434294ACE3AFB9031B7205A12245C26BA230247E6B870265A7DA3B0832D805ECE9C06D01A1CF5F3' +
    '59E25901C03796DD702767E46393920607E2099F0A56DB4B570D59BB41DB52C1FA73E1311E114352' +
    '7C697CFA06364FD3A1DCEBAE06A97FF3F39269280FD7975E17D9279119A80664503DD4297D03368D' +
    '111DFD719D97EC77F1F31F346B2526E344C1F95D2FC81CEE491CA8C461380B87E9544E86F735C528' +
    '2EC2268193093C68BC76F3DC300207344400A04DA5371C3ABE8D14A639E26F26D147FEAA1A7E5AC7' +
    '171A9A3578BCF4433293E6A9C106CD8E632E05B6AFC79BA075D75604F7709A847EFB29BA70C51FA4' +
    '839E02C99EB8B8648016C740B048C5827A13EB2A8FDA5402287D3D876FD4558F5696B0947411C4F6' +
    '1FC69A8CE3F4A76C04C00A201B31365D977784ED5C18E5A92A3E59A465866B09971C2562880A5643' +
    '8CA9391508A4B891304D752C41BB25C11682F08B9550E17E9E141D797B4EC9998EF7F17FB884354B' +
    'B4682544447523B887F8273A471C5980A50BF28D6B496E05A52B1DD5AE2CA95B479256BBE1278788' +
    '778D6727E64CA62FE548785285AAD126151A91822A621E8ABA3EDA27158B8E2310C540A9322CC56E' +
    '804C16261572CCB3B6AB2B6CC7876C034548369A5029944F2B03AA2F38CE18963CDA42AB5CF1096C' +
    'C81AEF738F9EE681A11C39D4C794F9497AB76A1392AC019C07B1CB0483BE85B1982741D229A92622' +
    'B721D73F27C6C90863150E7C33764C8329CB5323146EA050B9F75B4F6F597934A076AB0A8C44D884' +
    'AA89BC1D45812DEA5A14D720CF1141F0B269ECF8BA767A553FD63A63EA40D67B06E9A13FFB242958' +
    '77086932CC6BDABC2425920531B91A097CE8120C047B1F77977E14770FE409298B978A40464859EB' +
    '8D93914E493A2E6C902149F893A5D61FBCD318FBD9ACDCB0183721C8CA757C4E930AC72C5E170747' +
    '2E81B13BB0B0EE785E35F840B0EAB8EF5C7DD88C3E8DC9AA0B080E9BD1386C750693024B661458BC' +
    '2462EAC6B3EB08AA44501C8F1BB88F942D05AA445D421F028A2BFC42551822AB2980BD16578C3A75' +
    '428D64B9D2038887285EABF1ADF4918A52239DFE8A367C051B2D4C71B03108EA6B516EE877038BA2' +
    '2E45603287A81E554B75B8C57C02C9A558700BF44D9055644773B599A5ABB7202939477CB4F7159F' +
    '595A4F478AA6F25D8DFB8F271B521344BB1475C09327A5C7417882D8558D4B55504C42D2BA152E09' +
    '8F9864925C93842E225CB56176665446C6592ED0A40D36613FB6B33E96BA1F71D31306C26F865505' +
    '8C56228B7C8E508B033E339683344A0F86728160264C8894CF309FD827050C15CDC1CB102C673539' +
    '6C0683099414805A7892B44568916C4A59F5AB6F50418C54948ACB249C4F205E3257710BB087B45B' +
    '73A06A600B4534C629C07C915BBCBCBC3C8792347C079A821800B5361F1A7E5FE038127875237C73' +
    'AF529FF6C5080C660CD2B1505F081452BB7464852115299D3FD6CE8DBCB32AB7682362B38859B2AD' +
    'C4C6BD9269F13884BE8312B920A145B5691EEC639226CADB94C784B92074C61C7578947A7A2EE654' +
    '80D4C485A5335726F0A9B273C813CC16A64C1659A55E60B4A9DAD12CB38B33B4EA0F1DEC9613822E' +
    '93A29E93F91D5F240B2C0C6CFD940165042661A39B444B6F4D9A73017485CF6452912A3A98D93870' +
    '27C29A2A73F315BCE9B63859A988DE21815E93B593E09A83F362F647C9251A43D8384D54A262C23A' +
    '6CA4A69FE5C8AF9053404411A7B6D20978B939D567BC8FCA5DB6D54B24010E0F991CAF25C712612F' +
    'D3075BEE00746CE23C25059D725C44DBE4AC07C2BDE65148D6A02B73D2A8DBD0997EC86128C15B3F' +
    '12A615C2B9F5CB4DCF1B01E254715C075E876AAD336CB4E3986A39D8C410C9488E810439ABAE37D8' +
    '68BF7A33D87A384FA49DF0CB46E25A15007363361C85D85753779AC217569EDFF54644109BE7F252' +
    'D3A59B405074A7724FDB408736B5B459E7151B05057CACB5FC93A59078A0D4985327973A26AB058B' +
    'C41CDEB857311C301EFB0653817A3ED8A48E29865DFC22707760D31117F5D14070552295F361B757' +
    '0600839AB4C23B4BFB32FF7B0D0059128012A11552A2C73633909B68D76665125972B66BB73F6053' +
    'DA0C107E989F50A47243F934A5313CCE4978614794AE956293E165D22198A3220826367299524058' +
    '591841FC209123A36E492556C20039A09604834E90732D341C7CF7792557D3C05DCA2C5C66139C1A' +
    '5192B261246C3347A95AEBE227DE2747B2FC3C36508BB82248E604CE4D13570F452DC36A285AC465' +
    'D9B9791F4A52DC57013E449F9F098AFDAB740DE4B8676A5D3D238CD154ADE2B98EEE09B1BF5C1FBE' +
    'E70D495B5A08D5A7744248C90A0A5F9B87602C3A6E6C5E0044C3B22632A2C32191CA0DA0ACCD91B5' +
    '8858EA239C70353E8C576E383492204AD3BB141AABBA2FA199E2EC94A9B87E24B424A5F9CFC22674' +
    '1E00AC0BF0A648DAAA39A7571F2213F89A93BE88AF6DB80E7459CA754A0A9CD0912DD50FE3AAC42D' +
    '14416BA59EBFABABE0057A984849DDFB162C266C7544B42C43AA09930D7E76873FE879433B3EEC00' +
    '98FA5B4319D471184AB94C8474E508A06032BFC6612A8784199B483741C0B929195F00F395CC79BB' +
    'E058736439830FCC96BC5A2FD8A878C8B07355C02B46BB6833741187146F30525A7A149E52F3C87C' +
    '86C489655589D06767EBCEB1134538839DF2F81CF8977201296B96709D2E57AAF8215A07410C9258' +
    '54E91567BD78A09BD815E6F954FC932C444493F2CB801512C5AEDC8ACE9B1F0A02ADD6580885038F' +
    'B3519F85A4C8B473C2DBBC5F491358D907ADBE96958A1839BF371428BB59DA13A1968039580A30F5' +
    '1C6846B03066C624A40C0236529C59643AE741C0FF5C5EA4B3B527C53DBE26883ED84F3FF09F47EB' +
    '5DCF2A2AFA1BB598964E89CCABDAB75E5AA63AD9D42CBF56ABBD45798F1232C23462D2FB86FF066C' +
    '28828FC1012941333552D87D8ACAB64E82C1E3231C4834BC0D2650D4A96FF2C27D83D88FDF457B26' +
    'B77895469AC1A0A978C763DE93086136B1921AAB994769FB0697937420D1A398431C21E911379661' +
    '2F7A2A893DFAA09AD12F0DAC553F09C87C25A09DCC2BDEDBCBCC61508E26A0DC1099C2F530BC8756' +
    'D8CC7C6F134CE010381F16AE4F6A9E7E62907BF289C0B16882D71EEBE31BBDE1A45E0C69578548E3' +
    'D54E4A362E2C3A8E3FD5AF20115CFEB04258FC728C4BCC16D4C655CB7D0A783EC6DAAD3350BBAE53' +
    '009C608784742303F95599AB19197236691056E9687B0BC623B49A782E855DA6C8AF5695B2D1B605' +
    '60B2C12C0939F6E745F13895E3C672AA984183BA7B47AB70B26603FD84212602372DD49A99342D48' +
    'E0B573113FD2CCBA3EC57C79770FB22398533229B60B93C72397C1D1B34AC03E2BDA48836B49AE42' +
    'C5271786A63B9F542B9E64D106ECAC63740CB2E18BBC141B764AD5A6EE004AB80187A3904FD89324' +
    '97AB46C930293545A19750B6FC6C026E8C3B6B767795EB41D6800ADD791DB3871B6DE85BC39B5DDA' +
    'FB09BD459F6E3C3229A67A96F288A7B982B32630C9009009C2A0C6B2B1EAD47A033CABCDD687979C' +
    'CDB2160CB3653DB6B9B735317AEED78FCBB13E7BD38D0535B8D837653D91AFC70716BDC669333C36' +
    '3E4303A818B2FDC66A1672708CF9676832254C043535334408222A1232A6D92459E2500414832208' +
    '0AAD50B9BCD76B24D849C05E6825BA49B135C136EF8B4552A29DE0184BB9E6B50430892C0A0854F5' +
    '7ADC017E6844893F75025941123CCC83E4030DE481484EC0A1A9F896DD344A6C6BBC859B853AB47A' +
    '5BB51E97D04E391312A14268B66B6DEAD8A9077318DC8844F3F5B9D17A759E864F6209D0A95C3C34' +
    '5183EB5025AA6476210710FF5777D45924D302BF298A7FB3A54A402278E527AD946A6D745B5F58B8' +
    '86E3F48AC9EABB92CB840BF5CF907C6BA4434299C919659B6E4711725B47915720C7AD4A8DD5F16A' +
    'F653B742F6199AD94E2127748CE5BAB685CD23CC5156E8BE17F8786BF341A4D1A1A6D99CE9894A6B' +
    'E576C7DB7BCDFA6E5B95ACE6DB6A38BC51EEDBAE49D61ADBE43B63356C4B260481360B285A4B2650' +
    '1FC8B6E5709E5997EEB2359382FE949AD53490A5B2ED1093A6D261B9B64D69ADDD2512C0042D7AE6' +
    '7B530C1BE79FE1A9040BDE42DC8751FBECCE5CC47E5B1E488E2D8BF78172C9AB1E1D8F5ECAB9595D' +
    'C6C73BACD98DB0DD';
  S_CT =
    '11858DFD264FE4B86AAAF8553C8D658126DACD88FF3FD5B14CC68D3EC2A89B582814AE8403C0E9B3' +
    '8A706884682CE9F591083D6270A845A0656FAF228D597F0725A4B194EEDEDE5E2FD6D2A63AC3561B' +
    '66E043DBCDE7488052BA8F77F765842BB9A92B7D7DED2DFE7EBB635D28B2C7C252AD54848F282B86' +
    '6487D6248D1DB9CE9522C5E64B3FBAF38E35596C6DB2743DC48001F4B60D99BE025088F65F636571' +
    'A02DE3908D7E2ECD95DEE465BA4F711A8ADED49F223BC968921F60E84A3C5BB8839EADC6C8614C04' +
    'B5F964FF33A7F60CE68207474D792644D91A56003A2927E456C912F5B48BBCE0EDAF1B34F1B92585' +
    '9AEADF4AB88F40566390585BC646ED1D7662145EA1EB2C4E6D0430F709CE61B2131A646A9AB1E4C4' +
    '2508BB06A9698F894FDCB9239607022326D3E84B56026F35D6CBF7AFCEC479F68EFB3584F667BFF8' +
    'F77D9855498CD3825ED2199B9698DA56D7B830B6839AD331FB051D7124E7812D5707BDF481B5C1B0' +
    '80275E90D8F8424232F4E9DCAD38A21974EE6E3F402435D748B31E3350FE499A7E9A7D5DF1054E25' +
    'A1FA89E95F28FC03969A2777533605FD6B3CF7C2290271A1EFD3CE552935CBB124A2A19B62A2D04F' +
    '9342B13FD338BD1DD334F8536569339FAC3F282E34B0A4103C47803C6F01907295F50BEA35309906' +
    '87A79F17715FD9D88F7786CA04B37F14ED58DA7507C690FB6FD10141A8E37138A85947A04ABC332D' +
    '1A450780EAFE0A7E92BF4A250CB2659E9A92E3756459482A9ADE4645C7A2CBD98739E6E5957BEE39' +
    'BC321AA49F413CA1EF501C9CF0F5AC4C1609DC31D2A784CC7028AAACED78F4DCBA311698900C052C' +
    '416EA0AE24D62B154AE1746BFFD79A955FE3E33302212E0C71D6A4E9BDE343EA2A7F4D5F1F992964' +
    '64E22FD2F3F6BA74C0FDB3DC0AD7E5829E82A69FF1FF533E3E2995462273A68D4A82D29312095109' +
    '6C9A99C8708D120BA355234AF40F7AB49AAF5C5D8F55A43B3A0668D6391E9B10572938A25CE45261' +
    '769F7184F00FFD4A04A901A8E4033548F580E40473C3BADA634F912C4AF11F92584AAAB411AB5E07' +
    '97B207A123414FF37F6CF3AA5CF473AFF739FA562AC70A6E4B5C5248CCF4D14DDE0A1391EA18D56D' +
    'EEB9F167200E950383B18A04D3F844920C6106A2FEF1BE1B56FA076465027887A886463C60262DB5' +
    'FCCA785AB83B4A5CD268FFDCF82C48CDCF986661F03D2266521E31962AB98AF2DC81DC5BF0C68C24' +
    '14EEC4A2A1752B3EABFBFD2F06D0034202D19365F30B62C2ACE01D9DD9B92A9A8A25D9241E5346F1' +
    '402D3DB020894B256053123FA3D67BF2F9AF799D6F5A2240B731BE2EE204777988F532E0B48486DD' +
    '590860C15F0118ABF767AE2B55E2DB97EF09710EE0684B4E0761C494CB93F5780E8B07C87AA5150A' +
    '983D9971522978002F36E8F7706CDD53C195ED5DA61912CFBBB5D4E42C6C46CC532DB8BBFBD7CB2E' +
    'A31A9551579DCDB408E8EB4972A76064BFB9AB228B18D7B8314613EF7BCED857399044E24D4D07AA' +
    'A27CCAAE325883E8E518EA9E8EE79E58E5A9A25CF26251B3C422ACEEC5A270C0E905798482AE631B' +
    'A01DD63EFD4260E6D923F52E932B3406924B13C1FA26AC793C6BC959BC4AD5E9B55503CC16EE1824' +
    '6AEB2E7D9BE897BA49F1651CFFB962173AA1DB5DAA87DBEF9C8F59FB1C1F7429C9F139B3FEA911AF' +
    '87D08275DFB4683CF45350B810D4F3D8CF1526D574318C0341423D508B57C51575079CCC25E06407' +
    'A0AB4CE28310355E26398EE3573EB1365341E1486E8E372FA1B3B7CECC9FECE194DC5F38ADEBB7CB' +
    '73E6C312B549C1EE036E2E41E34089B286D092618CF9ACE6E1079D9DC8BA1B9DE0371D0C69647D9F' +
    '997010348352929A605877F9AB121A126C95DF5F46A89399B8D41AEBA9C290F99DFADB4A07DEE1B0' +
    '86BE595D9E0BE75B787C0166D6E2F3B529C6C309AC5BDD8396C39FF7AE3DF3AFBB9105DCD9754EF8' +
    'B7D330F6A159E9FF748695662F24C7480F1D2BB0DEC0AA212B3C85F54E33E428CEA7388AFF808DB7' +
    '7BB11BAB657639F7EB491780C286BD93ED1257A162D9D364A07A164A2967E98A0F53AB66B98FE555' +
    '262321DC3731093E7D1D713249F86997D48B18F83D355CCCDF2691757617EC3B184D9902460A8D9B' +
    'D551BEDD7D92FA9BE73022FB8085EA511038692A980AA13C0C263D755A2551EAC7AF73CA5C675ADF' +
    'EA6C0246CFB46DC5';
var
  M: TCnMLKEM;
  EK, DK, SK, CT: TBytes;
begin
  M := TCnMLKEM.Create(cmkt1024);

  try
    EK := HexToBytes(S_EK);
    DK := HexToBytes(S_DK);

    M.MLKEMEncaps(EK, HexToBytes('CF8B689629DE77D277026AA0A4E0256DBA2A78C9F0065362B7D83357E4A4EF0B'), SK, CT);
    Result := (BytesToHex(SK) = '7AF1B8176E9D3731BFD67EFCA8CF1BE35728D0681E7EDDE63F74B29BD16B7640') and
      (BytesToHex(CT) = S_CT);

    if not Result then Exit;

    SK := M.MLKEMDecaps(DK, CT);
    Result := BytesToHex(SK) = '7AF1B8176E9D3731BFD67EFCA8CF1BE35728D0681E7EDDE63F74B29BD16B7640';
  finally
    M.Free;
  end;
end;

// ================================ SM4 ========================================

function TestSM4Standard1: Boolean;
var
  S: string;
  KeyBytes, ResBytes, DataBytes: TBytes;
begin
  S := '0123456789ABCDEFFEDCBA9876543210';
  DataBytes := HexToBytes(S);
  KeyBytes := HexToBytes(S);
  ResBytes := SM4EncryptEcbBytes(KeyBytes, DataBytes);
  Result := BytesToHex(ResBytes) = '681EDF34D206965E86B3E94F536E4246';
end;

function TestSM4Standard2: Boolean;
var
  S: string;
  KeyBytes, DataBytes: TBytes;
  I: Integer;
begin
  S := '0123456789ABCDEFFEDCBA9876543210';
  DataBytes := HexToBytes(S);
  KeyBytes := HexToBytes(S);
  for I := 1 to 1000000 do
    DataBytes := SM4EncryptEcbBytes(KeyBytes, DataBytes);
  Result := BytesToHex(DataBytes) = '595298C7C6FD271F0402F804C33D3F66';
end;

function TestSM4Ecb: Boolean;
var
  S, Key, Res, Data: AnsiString;
begin
  S := 'CnPack Ecb Test Data for SM4.';
  Key := 'CnPack SM4 Key';
  SetLength(Res, SM4GetOutputLengthFromInputLength(Length(S)));
  SM4EncryptEcbStr(Key, S, @Res[1]);

  Result := DataToHex(@Res[1], Length(Res)) = 'CA1C161B95B8388398676525C4310ACDC608AD6DE2C57380BD593C2D406F40CC';
  if not Result then Exit;

  SetLength(Data, SM4GetOutputLengthFromInputLength(Length(Res)));
  SM4DecryptEcbStr(Key, Res, @Data[1]);

  Data := Trim(Data);
  Result := Data = S;
end;

function TestSM4Cbc: Boolean;
var
  KeyBytes, IvBytes, ResBytes, DataBytes: TBytes;
begin
  KeyBytes := AnsiToBytes('CnPack SM4 Key');
  IvBytes := AnsiToBytes('SM4 Iv Of CnPack');
  DataBytes := AnsiToBytes('CnPack Test Data for SM4 CBC.');
  ResBytes := SM4EncryptCbcBytes(KeyBytes, IvBytes, DataBytes);

  Result := BytesToHex(ResBytes) = 'FC752B7D3469AB7CE8F5FBA93452B4096901658D8669F43ECFF4A596B4CFC978';
  if not Result then Exit;

  ResBytes := SM4DecryptCbcBytes(KeyBytes, IvBytes, ResBytes);
  Result := CompareBytes(ResBytes, DataBytes, Length(DataBytes)); // 后面有 #0 要忽略
end;

function TestSM4Cfb: Boolean;
var
  KeyBytes, IvBytes, ResBytes, DataBytes: TBytes;
begin
  KeyBytes := AnsiToBytes('CnPack SM4 Key');
  IvBytes := AnsiToBytes('SM4 Iv Of CnPack');
  DataBytes := AnsiToBytes('CnPack Test Data for SM4 CFB.');
  ResBytes := SM4EncryptCfbBytes(KeyBytes, IvBytes, DataBytes);

  Result := BytesToHex(ResBytes) = '5BB273541D5464D7407BABDA8855CE5A8A1CD46C47393C9594BB1E3885';
  if not Result then Exit;

  ResBytes := SM4DecryptCfbBytes(KeyBytes, IvBytes, ResBytes);
  Result := CompareBytes(ResBytes, DataBytes);
end;

function TestSM4Ofb: Boolean;
var
  KeyBytes, IvBytes, ResBytes, DataBytes: TBytes;
begin
  KeyBytes := AnsiToBytes('SM4 Key CnPack');
  IvBytes := AnsiToBytes('SM4 CnPack Iv');
  DataBytes := AnsiToBytes('CnPack Test Data for SM4 OFB.');
  ResBytes := SM4EncryptOfbBytes(KeyBytes, IvBytes, DataBytes);

  Result := BytesToHex(ResBytes) = 'DC125402BEDEAC489E2430789D763498B536F81908A4F75279F2943476';
  if not Result then Exit;

  ResBytes := SM4DecryptOfbBytes(KeyBytes, IvBytes, ResBytes);
  Result := CompareBytes(ResBytes, DataBytes);
end;

function TestSM4Ctr: Boolean;
var
  KeyBytes, NonceBytes, ResBytes, DataBytes: TBytes;
begin
  KeyBytes := AnsiToBytes('SM4 Key CnPack');
  NonceBytes := AnsiToBytes('SM4Nonce');
  DataBytes := AnsiToBytes('CnPack Test Data for SM4 CTR.');
  ResBytes := SM4EncryptCtrBytes(KeyBytes, NonceBytes, DataBytes);

  Result := BytesToHex(ResBytes) = 'D959215B46C7A3B5AAC8646939051E1D52EF59952C557B8787AC536047';
  if not Result then Exit;

  ResBytes := SM4DecryptCtrBytes(KeyBytes, NonceBytes, ResBytes);
  Result := CompareBytes(ResBytes, DataBytes);
end;

// ================================ DES ========================================

function TestDESEcb: Boolean;
var
  S: string;
  KeyBytes, ResBytes, DataBytes: TBytes;
begin
  S := '0123456789ABCDEF';
  DataBytes := HexToBytes(S);
  KeyBytes := HexToBytes('133457799BBCDFF1');
  ResBytes := DESEncryptEcbBytes(KeyBytes, DataBytes);
  Result := BytesToHex(ResBytes) = '85E813540F0AB405';
end;

function TestDESCbc: Boolean;
var
  KeyBytes, IvBytes, ResBytes, DataBytes: TBytes;
begin
  KeyBytes := AnsiToBytes('Des Key');
  IvBytes := AnsiToBytes('Des Iv');
  DataBytes := AnsiToBytes('CnPack Test Data for DES CBC.');
  ResBytes := DESEncryptCBCBytes(KeyBytes, IvBytes, DataBytes);

  Result := BytesToHex(ResBytes) = '564AF4F43FF0F80C9C4BA18C2D2F6C1EBDA49AA749B26C3D06A2060CE6953A29';
  if not Result then Exit;

  ResBytes := DESDecryptCBCBytes(KeyBytes, IvBytes, ResBytes);
  Result := CompareBytes(ResBytes, DataBytes, Length(DataBytes));
end;

// ================================ 3DES =======================================

function Test3DESEcb: Boolean;
var
  S: string;
  KeyBytes, ResBytes, DataBytes: TBytes;
begin
  S := '0123456789ABCDEF';
  DataBytes := HexToBytes(S);
  KeyBytes := HexToBytes('9BBCDFF1AABBCCDD');
  ResBytes := TripleDESEncryptEcbBytes(KeyBytes, DataBytes);
  Result := BytesToHex(ResBytes) = '119102AA7D6000EE';
end;

function Test3DESCbc: Boolean;
var
  KeyBytes, IvBytes, ResBytes, DataBytes: TBytes;
begin
  KeyBytes := AnsiToBytes('3Des Key from CnPack');
  IvBytes := AnsiToBytes('3Des Iv');
  DataBytes := AnsiToBytes('CnPack Test Data for 3DES CBC.');
  ResBytes := TripleDESEncryptCBCBytes(KeyBytes, IvBytes, DataBytes);

  Result := BytesToHex(ResBytes) = 'E7C69043F789737DBDF122EFFB5BDBA149C0110F6E15CB63229339B95C750B8A';
  if not Result then Exit;

  ResBytes := TripleDESDecryptCBCBytes(KeyBytes, IvBytes, ResBytes);
  Result := CompareBytes(ResBytes, DataBytes, Length(DataBytes));
end;

// ================================ AES ========================================

function TestAESEcb128: Boolean;
var
  S: string;
  KeyBytes, ResBytes, DataBytes: TBytes;
begin
  S := '0123456789ABCDEF0123456789ABCDEF';
  DataBytes := HexToBytes(S);
  KeyBytes := HexToBytes('2B7E151628AED2A6ABF7158809CF4F3C');
  ResBytes := AESEncryptEcbBytes(DataBytes, KeyBytes, kbt128);
  Result := BytesToHex(ResBytes) = 'D44F0B792FD3B7C102A300501DBA089C';
end;

function TestAESEcb192: Boolean;
var
  S: string;
  KeyBytes, ResBytes, DataBytes: TBytes;
begin
  S := '0123456789ABCDEF0123456789ABCDEF';
  DataBytes := HexToBytes(S);
  KeyBytes := HexToBytes('8E73B0F7DA0E6452C810F32B809079E562F8EAD2522C6B7B');
  ResBytes := AESEncryptEcbBytes(DataBytes, KeyBytes, kbt192);
  Result := BytesToHex(ResBytes) = '5AF10516B9E7334485405B63C11EC1F4';
end;

function TestAESEcb256: Boolean;
var
  S: string;
  KeyBytes, ResBytes, DataBytes: TBytes;
begin
  S := '0123456789ABCDEF0123456789ABCDEF';
  DataBytes := HexToBytes(S);
  KeyBytes := HexToBytes('603DEB1015CA71BE2B73AEF0857D77811F352C073B6108D72D9810A30914DFF4');
  ResBytes := AESEncryptEcbBytes(DataBytes, KeyBytes, kbt256);
  Result := BytesToHex(ResBytes) = 'D71F96DEF80F6F19F80461CAEB8BE29F';
end;

function TestAESCbc128: Boolean;
var
  KeyBytes, IvBytes, ResBytes, DataBytes: TBytes;
begin
  KeyBytes := AnsiToBytes('CnPack AES Key');
  IvBytes := AnsiToBytes('AES Iv Of CnPack');
  DataBytes := AnsiToBytes('CnPack Test Data for AES CBC.');
  ResBytes := AESEncryptCbcBytes(DataBytes, KeyBytes, IvBytes, kbt128);

  Result := BytesToHex(ResBytes) = 'B3B163B21EBA050863BAC1A6FE39DD6EFF4D8EB5CBD60B5879FCE66558D2C69C';
  if not Result then Exit;

  ResBytes := AESDecryptCbcBytes(ResBytes, KeyBytes, IvBytes, kbt128);
  Result := CompareBytes(ResBytes, DataBytes, Length(DataBytes));
end;

function TestAESCbc192: Boolean;
var
  KeyBytes, IvBytes, ResBytes, DataBytes: TBytes;
begin
  KeyBytes := AnsiToBytes('CnPack AES Key');
  IvBytes := AnsiToBytes('AES Iv Of CnPack');
  DataBytes := AnsiToBytes('CnPack Test Data for AES CBC.');
  ResBytes := AESEncryptCbcBytes(DataBytes, KeyBytes, IvBytes, kbt192);

  Result := BytesToHex(ResBytes) = '7EE29DFBD7973F49760C92BC312F561F33587105F050BCB8C4558E175AACE840';
  if not Result then Exit;

  ResBytes := AESDecryptCbcBytes(ResBytes, KeyBytes, IvBytes, kbt192);
  Result := CompareBytes(ResBytes, DataBytes, Length(DataBytes));
end;

function TestAESCbc256: Boolean;
var
  KeyBytes, IvBytes, ResBytes, DataBytes: TBytes;
begin
  KeyBytes := AnsiToBytes('CnPack AES Key');
  IvBytes := AnsiToBytes('AES Iv Of CnPack');
  DataBytes := AnsiToBytes('CnPack Test Data for AES CBC.');
  ResBytes := AESEncryptCbcBytes(DataBytes, KeyBytes, IvBytes, kbt256);

  Result := BytesToHex(ResBytes) = '381D107404224569C3BC4CCAF71ECF312F188A12402241732A40EFAE69EA4587';
  if not Result then Exit;

  ResBytes := AESDecryptCbcBytes(ResBytes, KeyBytes, IvBytes, kbt256);
  Result := CompareBytes(ResBytes, DataBytes, Length(DataBytes));
end;

function TestAESCfb128: Boolean;
var
  KeyBytes, IvBytes, ResBytes, DataBytes: TBytes;
begin
  KeyBytes := AnsiToBytes('CnPack AES Key');
  IvBytes := AnsiToBytes('AES Iv Of CnPack');
  DataBytes := AnsiToBytes('CnPack Test Data for AES CFB.');
  ResBytes := AESEncryptCfbBytes(DataBytes, KeyBytes, IvBytes, kbt128);

  Result := BytesToHex(ResBytes) = 'D5CA4EFC7C656E63718283DBF9217ABC877EF21D9507B32147172683FB';
  if not Result then Exit;

  ResBytes := AESDecryptCfbBytes(ResBytes, KeyBytes, IvBytes, kbt128);
  Result := CompareBytes(ResBytes, DataBytes);
end;

function TestAESCfb192: Boolean;
var
  KeyBytes, IvBytes, ResBytes, DataBytes: TBytes;
begin
  KeyBytes := AnsiToBytes('CnPack AES Key');
  IvBytes := AnsiToBytes('AES Iv Of CnPack');
  DataBytes := AnsiToBytes('CnPack Test Data for AES CFB.');
  ResBytes := AESEncryptCfbBytes(DataBytes, KeyBytes, IvBytes, kbt192);

  Result := BytesToHex(ResBytes) = 'EAE9E836AFEED796377AD3A595C80FC43925777FADDC911CF3C094BCAB';
  if not Result then Exit;

  ResBytes := AESDecryptCfbBytes(ResBytes, KeyBytes, IvBytes, kbt192);
  Result := CompareBytes(ResBytes, DataBytes);
end;

function TestAESCfb256: Boolean;
var
  KeyBytes, IvBytes, ResBytes, DataBytes: TBytes;
begin
  KeyBytes := AnsiToBytes('CnPack AES Key');
  IvBytes := AnsiToBytes('AES Iv Of CnPack');
  DataBytes := AnsiToBytes('CnPack Test Data for AES CFB.');
  ResBytes := AESEncryptCfbBytes(DataBytes, KeyBytes, IvBytes, kbt256);

  Result := BytesToHex(ResBytes) = 'E5271041F97C434528E4426FA2CA3CD96994806B9765911657ABA87B00';
  if not Result then Exit;

  ResBytes := AESDecryptCfbBytes(ResBytes, KeyBytes, IvBytes, kbt256);
  Result := CompareBytes(ResBytes, DataBytes);
end;

function TestAESOfb128: Boolean;
var
  KeyBytes, IvBytes, ResBytes, DataBytes: TBytes;
begin
  KeyBytes := AnsiToBytes('CnPack AES Key');
  IvBytes := AnsiToBytes('AES Iv Of CnPack');
  DataBytes := AnsiToBytes('CnPack Test Data for AES OFB.');
  ResBytes := AESEncryptOfbBytes(DataBytes, KeyBytes, IvBytes, kbt128);

  Result := BytesToHex(ResBytes) = 'D5CA4EFC7C656E63718283DBF9217ABC5000A6506B556A87B173E6F37B';
  if not Result then Exit;

  ResBytes := AESDecryptOfbBytes(ResBytes, KeyBytes, IvBytes, kbt128);
  Result := CompareBytes(ResBytes, DataBytes);
end;

function TestAESOfb192: Boolean;
var
  KeyBytes, IvBytes, ResBytes, DataBytes: TBytes;
begin
  KeyBytes := AnsiToBytes('CnPack AES Key');
  IvBytes := AnsiToBytes('AES Iv Of CnPack');
  DataBytes := AnsiToBytes('CnPack Test Data for AES OFB.');
  ResBytes := AESEncryptOfbBytes(DataBytes, KeyBytes, IvBytes, kbt192);

  Result := BytesToHex(ResBytes) = 'EAE9E836AFEED796377AD3A595C80FC4B3BABCB7564945596F39082D59';
  if not Result then Exit;

  ResBytes := AESDecryptOfbBytes(ResBytes, KeyBytes, IvBytes, kbt192);
  Result := CompareBytes(ResBytes, DataBytes);
end;

function TestAESOfb256: Boolean;
var
  KeyBytes, IvBytes, ResBytes, DataBytes: TBytes;
begin
  KeyBytes := AnsiToBytes('CnPack AES Key');
  IvBytes := AnsiToBytes('AES Iv Of CnPack');
  DataBytes := AnsiToBytes('CnPack Test Data for AES OFB.');
  ResBytes := AESEncryptOfbBytes(DataBytes, KeyBytes, IvBytes, kbt256);

  Result := BytesToHex(ResBytes) = 'E5271041F97C434528E4426FA2CA3CD9DF7CFF961FEDD3F139A4108A1E';
  if not Result then Exit;

  ResBytes := AESDecryptOfbBytes(ResBytes, KeyBytes, IvBytes, kbt256);
  Result := CompareBytes(ResBytes, DataBytes);
end;

function TestAESCtr128: Boolean;
var
  KeyBytes, NonceBytes, IvBytes, ResBytes, DataBytes: TBytes;
begin
  // 来自 RFC 3686 的 TestVector 1
  KeyBytes := HexToBytes('AE6852F8121067CC4BF7A5765577F39E');
  NonceBytes := HexToBytes('00000030');
  IvBytes := HexToBytes('0000000000000000');
  DataBytes := HexToBytes('53696E676C6520626C6F636B206D7367');

  ResBytes := AESEncryptCtrBytes(DataBytes, KeyBytes, NonceBytes, IvBytes, kbt128);
  Result := BytesToHex(ResBytes) = 'E4095D4FB7A7B3792D6175A3261311B8';
  if not Result then Exit;

  ResBytes := AESDecryptCtrBytes(ResBytes, KeyBytes, NonceBytes, IvBytes, kbt128);
  Result := CompareBytes(ResBytes, DataBytes);
  if not Result then Exit;

  // 来自 RFC 3686 的 TestVector 2
  KeyBytes := HexToBytes('7E24067817FAE0D743D6CE1F32539163');
  NonceBytes := HexToBytes('006CB6DB');
  IvBytes := HexToBytes('C0543B59DA48D90B');
  DataBytes := HexToBytes('000102030405060708090A0B0C0D0E0F101112131415161718191A1B1C1D1E1F');

  ResBytes := AESEncryptCtrBytes(DataBytes, KeyBytes, NonceBytes, IvBytes, kbt128);
  Result := BytesToHex(ResBytes) = '5104A106168A72D9790D41EE8EDAD388EB2E1EFC46DA57C8FCE630DF9141BE28';
  if not Result then Exit;

  ResBytes := AESDecryptCtrBytes(ResBytes, KeyBytes, NonceBytes, IvBytes, kbt128);
  Result := CompareBytes(ResBytes, DataBytes);
  if not Result then Exit;

  // 来自 RFC 3686 的 TestVector 3
  KeyBytes := HexToBytes('7691BE035E5020A8AC6E618529F9A0DC');
  NonceBytes := HexToBytes('00E0017B');
  IvBytes := HexToBytes('27777F3F4A1786F0');
  DataBytes := HexToBytes('000102030405060708090A0B0C0D0E0F101112131415161718191A1B1C1D1E1F20212223');

  ResBytes := AESEncryptCtrBytes(DataBytes, KeyBytes, NonceBytes, IvBytes, kbt128);
  Result := BytesToHex(ResBytes) = 'C1CF48A89F2FFDD9CF4652E9EFDB72D74540A42BDE6D7836D59A5CEAAEF3105325B2072F';
  if not Result then Exit;

  ResBytes := AESDecryptCtrBytes(ResBytes, KeyBytes, NonceBytes, IvBytes, kbt128);
  Result := CompareBytes(ResBytes, DataBytes);
  if not Result then Exit;
end;

function TestAESCtr192: Boolean;
var
  KeyBytes, NonceBytes, IvBytes, ResBytes, DataBytes: TBytes;
begin
  // 来自 RFC 3686 的 TestVector 4
  KeyBytes := HexToBytes('16AF5B145FC9F579C175F93E3BFB0EED863D06CCFDB78515');
  NonceBytes := HexToBytes('00000048');
  IvBytes := HexToBytes('36733C147D6D93CB');
  DataBytes := HexToBytes('53696E676C6520626C6F636B206D7367');

  ResBytes := AESEncryptCtrBytes(DataBytes, KeyBytes, NonceBytes, IvBytes, kbt192);
  Result := BytesToHex(ResBytes) = '4B55384FE259C9C84E7935A003CBE928';
  if not Result then Exit;

  ResBytes := AESDecryptCtrBytes(ResBytes, KeyBytes, NonceBytes, IvBytes, kbt192);
  Result := CompareBytes(ResBytes, DataBytes);
  if not Result then Exit;

  // 来自 RFC 3686 的 TestVector 5
  KeyBytes := HexToBytes('7C5CB2401B3DC33C19E7340819E0F69C678C3DB8E6F6A91A');
  NonceBytes := HexToBytes('0096B03B');
  IvBytes := HexToBytes('020C6EADC2CB500D');
  DataBytes := HexToBytes('000102030405060708090A0B0C0D0E0F101112131415161718191A1B1C1D1E1F');

  ResBytes := AESEncryptCtrBytes(DataBytes, KeyBytes, NonceBytes, IvBytes, kbt192);
  Result := BytesToHex(ResBytes) = '453243FC609B23327EDFAAFA7131CD9F8490701C5AD4A79CFC1FE0FF42F4FB00';
  if not Result then Exit;

  ResBytes := AESDecryptCtrBytes(ResBytes, KeyBytes, NonceBytes, IvBytes, kbt192);
  Result := CompareBytes(ResBytes, DataBytes);
  if not Result then Exit;

  // 来自 RFC 3686 的 TestVector 6
  KeyBytes := HexToBytes('02BF391EE8ECB159B959617B0965279BF59B60A786D3E0FE');
  NonceBytes := HexToBytes('0007BDFD');
  IvBytes := HexToBytes('5CBD60278DCC0912');
  DataBytes := HexToBytes('000102030405060708090A0B0C0D0E0F101112131415161718191A1B1C1D1E1F20212223');

  ResBytes := AESEncryptCtrBytes(DataBytes, KeyBytes, NonceBytes, IvBytes, kbt192);
  Result := BytesToHex(ResBytes) = '96893FC55E5C722F540B7DD1DDF7E758D288BC95C69165884536C811662F2188ABEE0935';
  if not Result then Exit;

  ResBytes := AESDecryptCtrBytes(ResBytes, KeyBytes, NonceBytes, IvBytes, kbt192);
  Result := CompareBytes(ResBytes, DataBytes);
  if not Result then Exit;
end;

function TestAESCtr256: Boolean;
var
  KeyBytes, NonceBytes, IvBytes, ResBytes, DataBytes: TBytes;
begin
  // 来自 RFC 3686 的 TestVector 7
  KeyBytes := HexToBytes('776BEFF2851DB06F4C8A0542C8696F6C6A81AF1EEC96B4D37FC1D689E6C1C104');
  NonceBytes := HexToBytes('00000060');
  IvBytes := HexToBytes('DB5672C97AA8F0B2');
  DataBytes := HexToBytes('53696E676C6520626C6F636B206D7367');

  ResBytes := AESEncryptCtrBytes(DataBytes, KeyBytes, NonceBytes, IvBytes, kbt256);
  Result := BytesToHex(ResBytes) = '145AD01DBF824EC7560863DC71E3E0C0';
  if not Result then Exit;

  ResBytes := AESDecryptCtrBytes(ResBytes, KeyBytes, NonceBytes, IvBytes, kbt256);
  Result := CompareBytes(ResBytes, DataBytes);
  if not Result then Exit;

  // 来自 RFC 3686 的 TestVector 8
  KeyBytes := HexToBytes('F6D66D6BD52D59BB0796365879EFF886C66DD51A5B6A99744B50590C87A23884');
  NonceBytes := HexToBytes('00FAAC24');
  IvBytes := HexToBytes('C1585EF15A43D875');
  DataBytes := HexToBytes('000102030405060708090A0B0C0D0E0F101112131415161718191A1B1C1D1E1F');

  ResBytes := AESEncryptCtrBytes(DataBytes, KeyBytes, NonceBytes, IvBytes, kbt256);
  Result := BytesToHex(ResBytes) = 'F05E231B3894612C49EE000B804EB2A9B8306B508F839D6A5530831D9344AF1C';
  if not Result then Exit;

  ResBytes := AESDecryptCtrBytes(ResBytes, KeyBytes, NonceBytes, IvBytes, kbt256);
  Result := CompareBytes(ResBytes, DataBytes);
  if not Result then Exit;

  // 来自 RFC 3686 的 TestVector 9
  KeyBytes := HexToBytes('FF7A617CE69148E4F1726E2F43581DE2AA62D9F805532EDFF1EED687FB54153D');
  NonceBytes := HexToBytes('001CC5B7');
  IvBytes := HexToBytes('51A51D70A1C11148');
  DataBytes := HexToBytes('000102030405060708090A0B0C0D0E0F101112131415161718191A1B1C1D1E1F20212223');

  ResBytes := AESEncryptCtrBytes(DataBytes, KeyBytes, NonceBytes, IvBytes, kbt256);
  Result := BytesToHex(ResBytes) = 'EB6C52821D0BBBF7CE7594462ACA4FAAB407DF866569FD07F48CC0B583D6071F1EC0E6B8';
  if not Result then Exit;

  ResBytes := AESDecryptCtrBytes(ResBytes, KeyBytes, NonceBytes, IvBytes, kbt256);
  Result := CompareBytes(ResBytes, DataBytes);
  if not Result then Exit;
end;

// ================================ CRC ========================================

function TestCRC8CCITT: Boolean;
var
  S: AnsiString;
begin
  S := 'CnPack Test';
  Result := CRC8Calc(0, S[1], Length(S)) = $79;
end;

function TestCRC16CCITT: Boolean;
var
  S: AnsiString;
begin
  S := 'CnPack Test';
  Result := CRC16Calc(0, S[1], Length(S)) = $F352;
end;

function TestCRC32: Boolean;
var
  S: AnsiString;
begin
  S := 'CnPack Test';
  Result := CRC32Calc(0, S[1], Length(S)) = $C5B59359;
end;

function TestCRC64ECMA: Boolean;
var
  S: AnsiString;
begin
  S := 'CnPack Test';
  Result := CRC64Calc(0, S[1], Length(S)) = Int64($95CF1FEBBF05E07E);
  // 注意这里的结果对于 Int64 来说是负值，因此需要强制转换，否则 Linux64 下比较会不相等
end;

// ============================== XXHash =======================================

function TestXXH32: Boolean;
var
  Dig: TCnXXH32Digest;
  Data: TBytes;
begin
  Data := AnsiToBytes('CnPack Test');
  Dig := XXH32Bytes(Data);
  Result := DataToHex(@Dig[0], SizeOf(TCnXXH32Digest)) = '74514068';

  if not Result then Exit;

  Data := AnsiToBytes('CnPack TestCnPack TestCnPack TestCnPack TestCnPack TestCnPack TestCnPack TestCnPack TestCnPack TestCnPack Test');
  Dig := XXH32Bytes(Data);
  Result := DataToHex(@Dig[0], SizeOf(TCnXXH32Digest)) = 'FAB834F4';
end;

function TestXXH64: Boolean;
var
  Dig: TCnXXH64Digest;
  Data: TBytes;
begin
  Data := AnsiToBytes('CnPack Test');
  Dig := XXH64Bytes(Data);
  Result := DataToHex(@Dig[0], SizeOf(TCnXXH64Digest)) = '1639ECD44D3A4765';

  if not Result then Exit;

  Data := AnsiToBytes('CnPack TestCnPack TestCnPack TestCnPack TestCnPack TestCnPack TestCnPack TestCnPack TestCnPack TestCnPack Test');
  Dig := XXH64Bytes(Data);
  Result := DataToHex(@Dig[0], SizeOf(TCnXXH64Digest)) = '785BD894067C2F38';
end;

// ================================ MD5 ========================================

function TestMD5: Boolean;
var
  Dig: TCnMD5Digest;
  Data: TBytes;
begin
  Data := HexToBytes('436E5061636B2054657374');
  Dig := MD5Bytes(Data);
  Result := DataToHex(@Dig[0], SizeOf(TCnMD5Digest)) = '87E8D2C590172ED9590BF4D731C63759';
end;

function TestMD5Hmac: Boolean;
var
  S: AnsiString;
  Dig: TCnMD5Digest;
  Data: TBytes;
begin
  S := 'CnPack Key';
  Data := HexToBytes('436E5061636B2054657374');
  MD5Hmac(@S[1], Length(S), @Data[0], Length(Data), Dig);
  Result := DataToHex(@Dig[0], SizeOf(TCnMD5Digest)) = 'EE48551E4F54DFBAA43C65124FCCC675';
end;

function TestMD5Update: Boolean;
var
  D1, D2: TCnMD5Digest;
  C: TCnMD5Context;
  S, S1, S2: AnsiString;
begin
  S1 := '0123456789abcdefghi';
  S2 := 'jklmnop';
  S := S1 + S2;

  D1 := MD5StringA(S);
  MD5Init(C);
  MD5Update(C, PAnsiChar(S1), Length(S1));
  MD5Update(C, PAnsiChar(S2), Length(S2));
  MD5Final(C, D2);

  Result := MD5Match(D1, D2);
end;

// ================================ SHA1 =======================================

function TestSHA1: Boolean;
var
  Dig: TCnSHA1Digest;
  Data: TBytes;
begin
  Data := HexToBytes('436E5061636B2054657374');
  Dig := SHA1Bytes(Data);
  Result := DataToHex(@Dig[0], SizeOf(TCnSHA1Digest)) = 'B5AABEB0804C505196FE7CC5BBD5E298DA9D6C99';
end;

function TestSHA1HMac: Boolean;
var
  S: AnsiString;
  Dig: TCnSHA1Digest;
  Data: TBytes;
begin
  S := 'CnPack Key';
  Data := HexToBytes('436E5061636B2054657374');
  SHA1Hmac(@S[1], Length(S), @Data[0], Length(Data), Dig);
  Result := DataToHex(@Dig[0], SizeOf(TCnSHA1Digest)) = '1DD4E8CD93226D7D8253890260F62A4B8293766D';
end;

function TestSHA1Update: Boolean;
var
  D1, D2: TCnSHA1Digest;
  C: TCnSHA1Context;
  S, S1, S2: AnsiString;
begin
  S1 := '0123456789abcdefghi';
  S2 := 'jklmnop';
  S := S1 + S2;

  D1 := SHA1StringA(S);
  SHA1Init(C);
  SHA1Update(C, PAnsiChar(S1), Length(S1));
  SHA1Update(C, PAnsiChar(S2), Length(S2));
  SHA1Final(C, D2);

  Result := SHA1Match(D1, D2);
end;

// ================================ SHA2 =======================================

function TestSHA224: Boolean;
var
  Dig: TCnSHA224Digest;
  Data: TBytes;
begin
  Data := HexToBytes('436E5061636B2054657374');
  Dig := SHA224Bytes(Data);
  Result := DataToHex(@Dig[0], SizeOf(TCnSHA224Digest)) = '5740EE9ECBF5C9C682289710A797A27CE61F75F6959D757645A5BEB6';
end;

function TestSHA224HMac: Boolean;
var
  S: AnsiString;
  Dig: TCnSHA224Digest;
  Data: TBytes;
begin
  S := 'CnPack Key';
  Data := HexToBytes('436E5061636B2054657374');
  SHA224Hmac(@S[1], Length(S), @Data[0], Length(Data), Dig);
  Result := DataToHex(@Dig[0], SizeOf(TCnSHA224Digest)) = '33E0602F3BE8EEACA7C6F27B2158036FCD2D835893E0B22A158127C2';
end;

function TestSHA224Update: Boolean;
var
  D1, D2: TCnSHA224Digest;
  C: TCnSHA224Context;
  S, S1, S2: AnsiString;
begin
  S1 := '0123456789abcdefghi';
  S2 := 'jklmnop';
  S := S1 + S2;

  D1 := SHA224StringA(S);
  SHA224Init(C);
  SHA224Update(C, PAnsiChar(S1), Length(S1));
  SHA224Update(C, PAnsiChar(S2), Length(S2));
  SHA224Final(C, D2);

  Result := SHA224Match(D1, D2);
end;

function TestSHA256: Boolean;
var
  Dig: TCnSHA256Digest;
  Data: TBytes;
begin
  Data := HexToBytes('436E5061636B2054657374');
  Dig := SHA256Bytes(Data);
  Result := DataToHex(@Dig[0], SizeOf(TCnSHA256Digest)) = 'F1F9EA4FDD6E30B207743EAB5836207FE2ADD4B041C8CC2181FF6C58567D606C';
end;

function TestSHA256HMac: Boolean;
var
  S: AnsiString;
  Dig: TCnSHA256Digest;
  Data: TBytes;
begin
  S := 'CnPack Key';
  Data := HexToBytes('436E5061636B2054657374');
  SHA256Hmac(@S[1], Length(S), @Data[0], Length(Data), Dig);
  Result := DataToHex(@Dig[0], SizeOf(TCnSHA256Digest)) = 'DF8F5CA95CBF28996BD0A262084F539982FABCBEC3D6F2FF9CB6A31BE620E11C';
end;

function TestSHA256Update: Boolean;
var
  D1, D2: TCnSHA256Digest;
  C: TCnSHA256Context;
  S, S1, S2: AnsiString;
begin
  S1 := '0123456789abcdefghi';
  S2 := 'jklmnop';
  S := S1 + S2;

  D1 := SHA256StringA(S);
  SHA256Init(C);
  SHA256Update(C, PAnsiChar(S1), Length(S1));
  SHA256Update(C, PAnsiChar(S2), Length(S2));
  SHA256Final(C, D2);

  Result := SHA256Match(D1, D2);
end;

function TestSHA384: Boolean;
var
  Dig: TCnSHA384Digest;
  Data: TBytes;
begin
  Data := HexToBytes('436E5061636B2054657374');
  Dig := SHA384Bytes(Data);
  Result := DataToHex(@Dig[0], SizeOf(TCnSHA384Digest)) = 'CD9140D47932B7169483561B583EB6E63BB9BE117EB213F0CC8BD186305E8D6CF7078ED618CC197DB3808BE113C6FBA0';
end;

function TestSHA384HMac: Boolean;
var
  S: AnsiString;
  Dig: TCnSHA384Digest;
  Data: TBytes;
begin
  S := 'CnPack Key';
  Data := HexToBytes('436E5061636B2054657374');
  SHA384Hmac(@S[1], Length(S), @Data[0], Length(Data), Dig);
  Result := DataToHex(@Dig[0], SizeOf(TCnSHA384Digest)) = '3EC487D5A1E6011585C9AE5582E12DDA154D48C52851FE2633176B92FF8A6A08DE024617E641968D6D891719442BB082';
end;

function TestSHA384Update: Boolean;
var
  D1, D2: TCnSHA384Digest;
  C: TCnSHA384Context;
  S, S1, S2: AnsiString;
begin
  S1 := '0123456789abcdefghi';
  S2 := 'jklmnop';
  S := S1 + S2;

  D1 := SHA384StringA(S);
  SHA384Init(C);
  SHA384Update(C, PAnsiChar(S1), Length(S1));
  SHA384Update(C, PAnsiChar(S2), Length(S2));
  SHA384Final(C, D2);

  Result := SHA384Match(D1, D2);
end;

function TestSHA512: Boolean;
var
  Dig: TCnSHA512Digest;
  Data: TBytes;
begin
  Data := HexToBytes('436E5061636B2054657374');
  Dig := SHA512Bytes(Data);
  Result := DataToHex(@Dig[0], SizeOf(TCnSHA512Digest)) = '2181565A137A78603008FFAEEF25625B12BA003D7DE1937455559484FDA8DBD266AEB9478AF9F805B2C7E84DED752664BBDC4F023A40A5CB388ACCE8C4DE9E01';
end;

function TestSHA512HMac: Boolean;
var
  S: AnsiString;
  Dig: TCnSHA512Digest;
  Data: TBytes;
begin
  S := 'CnPack Key';
  Data := HexToBytes('436E5061636B2054657374');
  SHA512Hmac(@S[1], Length(S), @Data[0], Length(Data), Dig);
  Result := DataToHex(@Dig[0], SizeOf(TCnSHA512Digest)) = 'DBBBEE460673F447B39CC9F72C2C23361281497834F2830BBCF56F1325282172303B9DB2F88D61AF0EEE5997D3035E2CFA9DF7E57B8FE77B0F9F694318C18E46';
end;

function TestSHA512Update: Boolean;
var
  D1, D2: TCnSHA512Digest;
  C: TCnSHA512Context;
  S, S1, S2: AnsiString;
begin
  S1 := '0123456789abcdefghi';
  S2 := 'jklmnop';
  S := S1 + S2;

  D1 := SHA512StringA(S);
  SHA512Init(C);
  SHA512Update(C, PAnsiChar(S1), Length(S1));
  SHA512Update(C, PAnsiChar(S2), Length(S2));
  SHA512Final(C, D2);

  Result := SHA512Match(D1, D2);
end;

// ================================ SHA3 =======================================

function TestSHA3_224: Boolean;
var
  Dig: TCnSHA3_224Digest;
  Data: TBytes;
begin
  Data := HexToBytes('436E5061636B2054657374');
  Dig := SHA3_224Bytes(Data);
  Result := DataToHex(@Dig[0], SizeOf(TCnSHA3_224Digest)) = '95CAE72FFEE9915A8745A2E5170DB4D38A1CEA2EAF252075D0611784';
end;

function TestSHA3_224HMac: Boolean;
var
  S: AnsiString;
  Dig: TCnSHA3_224Digest;
  Data: TBytes;
begin
  S := 'CnPack Key';
  Data := HexToBytes('436E5061636B2054657374');
  SHA3_224Hmac(@S[1], Length(S), @Data[0], Length(Data), Dig);
  Result := DataToHex(@Dig[0], SizeOf(TCnSHA3_224Digest)) = 'A20F92973578642EC6A841EAB0AA4091C24E7629715D656C006E0E53';
end;

function TestSHA3_224Update: Boolean;
var
  D1, D2: TCnSHA3_224Digest;
  C: TCnSHA3Context;
  S, S1, S2: AnsiString;
begin
  S1 := '0123456789abcdefghi';
  S2 := 'jklmnop';
  S := S1 + S2;

  D1 := SHA3_224StringA(S);
  SHA3_224Init(C);
  SHA3_224Update(C, PAnsiChar(S1), Length(S1));
  SHA3_224Update(C, PAnsiChar(S2), Length(S2));
  SHA3_224Final(C, D2);

  Result := SHA3_224Match(D1, D2);
end;

function TestSHA3_256: Boolean;
var
  Dig: TCnSHA3_256Digest;
  Data: TBytes;
begin
  Data := HexToBytes('436E5061636B2054657374');
  Dig := SHA3_256Bytes(Data);
  Result := DataToHex(@Dig[0], SizeOf(TCnSHA3_256Digest)) = '02C7DBCCD45CCFD93B77FD679636472C5A6547D48710EFF3F02E8D26C8A80396';
end;

function TestSHA3_256HMac: Boolean;
var
  S: AnsiString;
  Dig: TCnSHA3_256Digest;
  Data: TBytes;
begin
  S := 'CnPack Key';
  Data := HexToBytes('436E5061636B2054657374');
  SHA3_256Hmac(@S[1], Length(S), @Data[0], Length(Data), Dig);
  Result := DataToHex(@Dig[0], SizeOf(TCnSHA3_256Digest)) = 'FFF6F1CA3728ADB22D5E2B07B302BE522AE62A5D3711841E0C0A0F483AEC8DCE';
end;

function TestSHA3_256Update: Boolean;
var
  D1, D2: TCnSHA3_256Digest;
  C: TCnSHA3Context;
  S, S1, S2: AnsiString;
begin
  S1 := '0123456789abcdefghi';
  S2 := 'jklmnop';
  S := S1 + S2;

  D1 := SHA3_256StringA(S);
  SHA3_256Init(C);
  SHA3_256Update(C, PAnsiChar(S1), Length(S1));
  SHA3_256Update(C, PAnsiChar(S2), Length(S2));
  SHA3_256Final(C, D2);

  Result := SHA3_256Match(D1, D2);
end;

function TestSHA3_384: Boolean;
var
  Dig: TCnSHA3_384Digest;
  Data: TBytes;
begin
  Data := HexToBytes('436E5061636B2054657374');
  Dig := SHA3_384Bytes(Data);
  Result := DataToHex(@Dig[0], SizeOf(TCnSHA3_384Digest)) = '4B2591066C2988CE32478A0EC3985E34A19161E51EBBAF589C5E94AC88443E7CE27A998592626615444F6829F2966E87';
end;

function TestSHA3_384HMac: Boolean;
var
  S: AnsiString;
  Dig: TCnSHA3_384Digest;
  Data: TBytes;
begin
  S := 'CnPack Key';
  Data := HexToBytes('436E5061636B2054657374');
  SHA3_384Hmac(@S[1], Length(S), @Data[0], Length(Data), Dig);
  Result := DataToHex(@Dig[0], SizeOf(TCnSHA3_384Digest)) = 'DB48EE0068F826CC97D0B305DCC1C726662C4EE428404F7BC923DC14142E1D12050D55355AD784046F2C848323F67832';
end;

function TestSHA3_384Update: Boolean;
var
  D1, D2: TCnSHA3_384Digest;
  C: TCnSHA3Context;
  S, S1, S2: AnsiString;
begin
  S1 := '0123456789abcdefghi';
  S2 := 'jklmnop';
  S := S1 + S2;

  D1 := SHA3_384StringA(S);
  SHA3_384Init(C);
  SHA3_384Update(C, PAnsiChar(S1), Length(S1));
  SHA3_384Update(C, PAnsiChar(S2), Length(S2));
  SHA3_384Final(C, D2);

  Result := SHA3_384Match(D1, D2);
end;

function TestSHA3_512: Boolean;
var
  Dig: TCnSHA3_512Digest;
  Data: TBytes;
begin
  Data := HexToBytes('436E5061636B2054657374');
  Dig := SHA3_512Bytes(Data);
  Result := DataToHex(@Dig[0], SizeOf(TCnSHA3_512Digest)) = '7A71323C66B4C7C3A6FAEA8FB38824914B3CEBF0FE95B550F0F3281F3A6B0F30D2B60E405CDB1E3B9C87A7B0986DFA6234685DEDF3271DABE840D9146186B281';
end;

function TestSHA3_512HMac: Boolean;
var
  S: AnsiString;
  Dig: TCnSHA3_512Digest;
  Data: TBytes;
begin
  S := 'CnPack Key';
  Data := HexToBytes('436E5061636B2054657374');
  SHA3_512Hmac(@S[1], Length(S), @Data[0], Length(Data), Dig);
  Result := DataToHex(@Dig[0], SizeOf(TCnSHA3_512Digest)) = 'F97C8E267641A64BD1DF46A6EBB032F53C76DF3DC6D549201235CC499A0974189D712503B3DE023C96F5CBA36F021AD31BD0FF809D67FEF220BE32F42848247E';
end;

function TestSHA3_512Update: Boolean;
var
  D1, D2: TCnSHA3_512Digest;
  C: TCnSHA3Context;
  S, S1, S2: AnsiString;
begin
  S1 := '0123456789abcdefghi';
  S2 := 'jklmnop';
  S := S1 + S2;

  D1 := SHA3_512StringA(S);
  SHA3_512Init(C);
  SHA3_512Update(C, PAnsiChar(S1), Length(S1));
  SHA3_512Update(C, PAnsiChar(S2), Length(S2));
  SHA3_512Final(C, D2);

  Result := SHA3_512Match(D1, D2);
end;

function TestSHAKE1281: Boolean;
var
  S, R: TBytes;
begin
  // 例子来源于 NIST 的 SHAKE128_Msg160 Example
  SetLength(S, 200);
  FillChar(S[0], Length(S), $A3);
  R := SHAKE128Bytes(S, 32 * 16);
  Result := DataToHex(@R[0], Length(R)) =
    '131AB8D2B594946B9C81333F9BB6E0CE' +
    '75C3B93104FA3469D3917457385DA037' +
    'CF232EF7164A6D1EB448C8908186AD85' +
    '2D3F85A5CF28DA1AB6FE343817197846' +
    '7F1C05D58C7EF38C284C41F6C2221A76' +
    'F12AB1C04082660250802294FB871802' +
    '13FDEF5B0ECB7DF50CA1F8555BE14D32' +
    'E10F6EDCDE892C09424B29F597AFC270' +
    'C904556BFCB47A7D40778D390923642B' +
    '3CBD0579E60908D5A000C1D08B98EF93' +
    '3F806445BF87F8B009BA9E94F7266122' +
    'ED7AC24E5E266C42A82FA1BBEFB7B8DB' +
    '0066E16A85E0493F07DF4809AEC084A5' +
    '93748AC3DDE5A6D7AAE1E8B6E5352B2D' +
    '71EFBB47D4CAEED5E6D633805D2D323E' +
    '6FD81B4684B93A2677D45E7421C2C6AE' +
    'A259B855A698FD7D13477A1FE53E5A4A' +
    '6197DBEC5CE95F505B520BCD9570C4A8' +
    '265A7E01F89C0C002C59BFEC6CD4A5C1' +
    '09258953EE5EE70CD577EE217AF21FA7' +
    '0178F0946C9BF6CA8751793479F6B537' +
    '737E40B6ED28511D8A2D7E73EB75F8DA' +
    'AC912FF906E0AB955B083BAC45A8E5E9' +
    'B744C8506F37E9B4E749A184B30F43EB' +
    '188D855F1B70D71FF3E50C537AC1B0F8' +
    '974F0FE1A6AD295BA42F6AEC74D123A7' +
    'ABEDDE6E2C0711CAB36BE5ACB1A5A11A' +
    '4B1DB08BA6982EFCCD716929A7741CFC' +
    '63AA4435E0B69A9063E880795C3DC5EF' +
    '3272E11C497A91ACF699FEFEE206227A' +
    '44C9FB359FD56AC0A9A75A743CFF6862' +
    'F17D7259AB075216C0699511643B6439';
end;

function TestSHAKE1282: Boolean;
var
  Ctx: TCnSHA3Context;
  S, R: TBytes;
  K: string;
begin
  SHAKE128Init(Ctx, 0);

  S := HexToBytes('1FB5');
  SHAKE128Absorb(Ctx, PAnsiChar(@S[0]), Length(S));
  S := HexToBytes('C1278524D02645EF90C0219FF571');
  SHAKE128Absorb(Ctx, PAnsiChar(@S[0]), Length(S));

  R := SHAKE128Squeeze(Ctx, 20);
  K := BytesToHex(R);
  R := SHAKE128Squeeze(Ctx, 37);
  K := K + BytesToHex(R);

  Result := K = 'FEBCA33E3157EC869E0F138A2E4BAA78F122ECB2E1A929145C9AC9A35E5B8378CC877F87EA05426EFEBAD962313BDC6018B5F6A9CCFA1D4960';
end;

function TestSHAKE2561: Boolean;
var
  S, R: TBytes;
begin
  // 例子来源于 NIST 的 SHAKE256_Msg160 Example
  SetLength(S, 200);
  FillChar(S[0], Length(S), $A3);
  R := SHAKE256Bytes(S, 32 * 16);
  Result := DataToHex(@R[0], Length(R)) =
    'CD8A920ED141AA0407A22D59288652E9' +
    'D9F1A7EE0C1E7C1CA699424DA84A904D' +
    '2D700CAAE7396ECE96604440577DA4F3' +
    'AA22AEB8857F961C4CD8E06F0AE6610B' +
    '1048A7F64E1074CD629E85AD7566048E' +
    'FC4FB500B486A3309A8F26724C0ED628' +
    '001A1099422468DE726F1061D99EB9E9' +
    '3604D5AA7467D4B1BD6484582A384317' +
    'D7F47D750B8F5499512BB85A226C4243' +
    '556E696F6BD072C5AA2D9B69730244B5' +
    '6853D16970AD817E213E470618178001' +
    'C9FB56C54FEFA5FEE67D2DA524BB3B0B' +
    '61EF0E9114A92CDBB6CCCB98615CFE76' +
    'E3510DD88D1CC28FF99287512F24BFAF' +
    'A1A76877B6F37198E3A641C68A7C42D4' +
    '5FA7ACC10DAE5F3CEFB7B735F12D4E58' +
    '9F7A456E78C0F5E4C4471FFFA5E4FA05' +
    '14AE974D8C2648513B5DB494CEA84715' +
    '6D277AD0E141C24C7839064CD08851BC' +
    '2E7CA109FD4E251C35BB0A04FB05B364' +
    'FF8C4D8B59BC303E25328C09A882E952' +
    '518E1A8AE0FF265D61C465896973D749' +
    '0499DC639FB8502B39456791B1B6EC5B' +
    'CC5D9AC36A6DF622A070D43FED781F5F' +
    '149F7B62675E7D1A4D6DEC48C1C71645' +
    '86EAE06A51208C0B791244D307726505' +
    'C3AD4B26B6822377257AA152037560A7' +
    '39714A3CA79BD605547C9B78DD1F596F' +
    '2D4F1791BC689A0E9B799A37339C0427' +
    '5733740143EF5D2B58B96A363D4E0807' +
    '6A1A9D7846436E4DCA5728B6F760EEF0' +
    'CA92BF0BE5615E96959D767197A0BEEB';
end;

function TestSHAKE2562: Boolean;
var
  Ctx: TCnSHA3Context;
  S, R: TBytes;
  K: string;
begin
  SHAKE256Init(Ctx, 0);

  S := HexToBytes('7D9312FFE94845AC51056C63EB3BFF4A94626AAFB7470FF86FA88F');
  SHAKE256Absorb(Ctx, PAnsiChar(@S[0]), Length(S));
  S := HexToBytes('D8F0FE45C9');
  SHAKE256Absorb(Ctx, PAnsiChar(@S[0]), Length(S));

  R := SHAKE128Squeeze(Ctx, 1);
  K := BytesToHex(R);
  R := SHAKE128Squeeze(Ctx, 20);
  K := K + BytesToHex(R);
  R := SHAKE128Squeeze(Ctx, 50);
  K := K + BytesToHex(R);
  R := SHAKE128Squeeze(Ctx, 38);
  K := K + BytesToHex(R);

  Result := K = 'DE489392796FD3B530C506E482936AFCFE6B72DCF7E9DEF054953842FF' +
    '19076908C8A1D6A4E7639E0FDBFA1B5201095051AAC3E3997779E588377EAC979313E3' +
    '9C3721DC9F912CF7FDF1A9038CBABA8E9F3D95951A5D819BFFD0B080319FCD12DA0516' +
    'BAF54B779E79E437D3EC';
end;

// ================================ BLAKE ======================================

function TestBLAKE224: Boolean;
var
  Dig: TCnBLAKE224Digest;
  Data: TBytes;
begin
  Dig := BLAKE224Bytes(nil);
  Result := DataToHex(@Dig[0], SizeOf(TCnBLAKE224Digest)) = '7DC5313B1C04512A174BD6503B89607AECBEE0903D40A8A569C94EED';

  if not Result then Exit;

  Data := HexToBytes('00');
  Dig := BLAKE224Bytes(Data);
  Result := DataToHex(@Dig[0], SizeOf(TCnBLAKE224Digest)) = '4504CB0314FB2A4F7A692E696E487912FE3F2468FE312C73A5278EC5';

  if not Result then Exit;

  Data := HexToBytes('000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000');
  Dig := BLAKE224Bytes(Data);
  Result := DataToHex(@Dig[0], SizeOf(TCnBLAKE224Digest)) = 'F5AA00DD1CB847E3140372AF7B5C46B4888D82C8C0A917913CFB5D04';
end;

function TestBLAKE224HMac: Boolean;
var
  S: AnsiString;
  Dig: TCnBLAKE224Digest;
  Data: TBytes;
begin
  S := 'CnPack Key';
  Data := HexToBytes('436E5061636B2054657374');
  BLAKE224Hmac(@S[1], Length(S), @Data[0], Length(Data), Dig);
  Result := DataToHex(@Dig[0], SizeOf(TCnBLAKE224Digest)) = '6E680E83717D0A06A787D4E310D423942B05EAA107A803A005584A69';
end;

function TestBLAKE224Update: Boolean;
var
  D1, D2: TCnBLAKE224Digest;
  C: TCnBLAKE224Context;
  S, S1, S2: AnsiString;
begin
  S1 := '0123456789abcdefghi';
  S2 := 'jklmnop';
  S := S1 + S2;

  D1 := BLAKE224StringA(S);
  BLAKE224Init(C);
  BLAKE224Update(C, PAnsiChar(S1), Length(S1));
  BLAKE224Update(C, PAnsiChar(S2), Length(S2));
  BLAKE224Final(C, D2);

  Result := BLAKE224Match(D1, D2);
end;

function TestBLAKE256: Boolean;
var
  Dig: TCnBLAKE256Digest;
  Data: TBytes;
begin
  Dig := BLAKE256Bytes(nil);
  Result := DataToHex(@Dig[0], SizeOf(TCnBLAKE256Digest)) = '716F6E863F744B9AC22C97EC7B76EA5F5908BC5B2F67C61510BFC4751384EA7A';

  if not Result then Exit;

  Data := HexToBytes('00');
  Dig := BLAKE256Bytes(Data);
  Result := DataToHex(@Dig[0], SizeOf(TCnBLAKE256Digest)) = '0CE8D4EF4DD7CD8D62DFDED9D4EDB0A774AE6A41929A74DA23109E8F11139C87';

  if not Result then Exit;

  Data := HexToBytes('000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000');
  Dig := BLAKE256Bytes(Data);
  Result := DataToHex(@Dig[0], SizeOf(TCnBLAKE256Digest)) = 'D419BAD32D504FB7D44D460C42C5593FE544FA4C135DEC31E21BD9ABDCC22D41';
end;

function TestBLAKE256HMac: Boolean;
var
  S: AnsiString;
  Dig: TCnBLAKE256Digest;
  Data: TBytes;
begin
  S := 'CnPack Key';
  Data := HexToBytes('436E5061636B2054657374');
  BLAKE256Hmac(@S[1], Length(S), @Data[0], Length(Data), Dig);
  Result := DataToHex(@Dig[0], SizeOf(TCnBLAKE256Digest)) = '5FA4F8D8187F34BFA24ED840E4C9BB72830617D19CDE6BF9C816BE593AE5B61E';
end;

function TestBLAKE256Update: Boolean;
var
  D1, D2: TCnBLAKE256Digest;
  C: TCnBLAKE256Context;
  S, S1, S2: AnsiString;
begin
  S1 := '0123456789abcdefghi';
  S2 := 'jklmnop';
  S := S1 + S2;

  D1 := BLAKE256StringA(S);
  BLAKE256Init(C);
  BLAKE256Update(C, PAnsiChar(S1), Length(S1));
  BLAKE256Update(C, PAnsiChar(S2), Length(S2));
  BLAKE256Final(C, D2);

  Result := BLAKE256Match(D1, D2);
end;

function TestBLAKE384: Boolean;
var
  Dig: TCnBLAKE384Digest;
  Data: TBytes;
begin
  Dig := BLAKE384Bytes(nil);
  Result := DataToHex(@Dig[0], SizeOf(TCnBLAKE384Digest)) = 'C6CBD89C926AB525C242E6621F2F5FA73AA4AFE3D9E24AED727FAAADD6AF38B620BDB623DD2B4788B1C8086984AF8706';

  if not Result then Exit;

  Data := HexToBytes('00');
  Dig := BLAKE384Bytes(Data);
  Result := DataToHex(@Dig[0], SizeOf(TCnBLAKE384Digest)) = '10281F67E135E90AE8E882251A355510A719367AD70227B137343E1BC122015C29391E8545B5272D13A7C2879DA3D807';

  if not Result then Exit;

  Data := HexToBytes('000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000'
    + '000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000');
  Dig := BLAKE384Bytes(Data);
  Result := DataToHex(@Dig[0], SizeOf(TCnBLAKE384Digest)) = '0B9845DD429566CDAB772BA195D271EFFE2D0211F16991D766BA749447C5CDE569780B2DAA66C4B224A2EC2E5D09174C';
end;

function TestBLAKE384HMac: Boolean;
var
  S: AnsiString;
  Dig: TCnBLAKE384Digest;
  Data: TBytes;
begin
  S := 'CnPack Key';
  Data := HexToBytes('436E5061636B2054657374');
  BLAKE384Hmac(@S[1], Length(S), @Data[0], Length(Data), Dig);
  Result := DataToHex(@Dig[0], SizeOf(TCnBLAKE384Digest)) = 'EAB03631FF5C841689BE189A227AC263317DC40247C8DAD81D71678A7A07F98AEF6C970DD8AD2E752D308173ABDC02F2';
end;

function TestBLAKE384Update: Boolean;
var
  D1, D2: TCnBLAKE384Digest;
  C: TCnBLAKE384Context;
  S, S1, S2: AnsiString;
begin
  S1 := '0123456789abcdefghi';
  S2 := 'jklmnop';
  S := S1 + S2;

  D1 := BLAKE384StringA(S);
  BLAKE384Init(C);
  BLAKE384Update(C, PAnsiChar(S1), Length(S1));
  BLAKE384Update(C, PAnsiChar(S2), Length(S2));
  BLAKE384Final(C, D2);

  Result := BLAKE384Match(D1, D2);
end;

function TestBLAKE512: Boolean;
var
  Dig: TCnBLAKE512Digest;
  Data: TBytes;
begin
  Dig := BLAKE512Bytes(nil);
  Result := DataToHex(@Dig[0], SizeOf(TCnBLAKE512Digest)) = 'A8CFBBD73726062DF0C6864DDA65DEFE58EF0CC52A5625090FA17601E1EECD1B628E94F396AE402A00ACC9EAB77B4D4C2E852AAAA25A636D80AF3FC7913EF5B8';

  if not Result then Exit;

  Data := HexToBytes('00');
  Dig := BLAKE512Bytes(Data);
  Result := DataToHex(@Dig[0], SizeOf(TCnBLAKE512Digest)) = '97961587F6D970FABA6D2478045DE6D1FABD09B61AE50932054D52BC29D31BE4FF9102B9F69E2BBDB83BE13D4B9C06091E5FA0B48BD081B634058BE0EC49BEB3';

  if not Result then Exit;

  Data := HexToBytes('000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000'
    + '000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000');
  Dig := BLAKE512Bytes(Data);
  Result := DataToHex(@Dig[0], SizeOf(TCnBLAKE512Digest)) = '313717D608E9CF758DCB1EB0F0C3CF9FC150B2D500FB33F51C52AFC99D358A2F1374B8A38BBA7974E7F6EF79CAB16F22CE1E649D6E01AD9589C213045D545DDE';
end;

function TestBLAKE512HMac: Boolean;
var
  S: AnsiString;
  Dig: TCnBLAKE512Digest;
  Data: TBytes;
begin
  S := 'CnPack Key';
  Data := HexToBytes('436E5061636B2054657374');
  BLAKE512Hmac(@S[1], Length(S), @Data[0], Length(Data), Dig);
  Result := DataToHex(@Dig[0], SizeOf(TCnBLAKE512Digest)) = '8BDC51AE7542AC47E32E60659ADB57C53D4CD2EAEFA28911ECDFB6F2419F97539AC73C3DCBF4051782968DA0B8914A5C2DA423762E56C6208E0A12DA0E30EA90';
end;

function TestBLAKE512Update: Boolean;
var
  D1, D2: TCnBLAKE512Digest;
  C: TCnBLAKE512Context;
  S, S1, S2: AnsiString;
begin
  S1 := '0123456789abcdefghi';
  S2 := 'jklmnop';
  S := S1 + S2;

  D1 := BLAKE512StringA(S);
  BLAKE512Init(C);
  BLAKE512Update(C, PAnsiChar(S1), Length(S1));
  BLAKE512Update(C, PAnsiChar(S2), Length(S2));
  BLAKE512Final(C, D2);

  Result := BLAKE512Match(D1, D2);
end;

// =============================== BLAKE2 ======================================

function TestBLAKE2S: Boolean;
var
  Dig: TCnBLAKE2SDigest;
  Data: TBytes;
begin
  Dig := BLAKE2SBytes(nil);
  Result := DataToHex(@Dig[0], SizeOf(TCnBLAKE2SDigest)) = '69217A3079908094E11121D042354A7C1F55B6482CA1A51E1B250DFD1ED0EEF9';

  if not Result then Exit;

  Dig := BLAKE2SBytes(nil, nil, 28);
  Result := DataToHex(@Dig[0], 28) = '1FA1291E65248B37B3433475B2A0DD63D54A11ECC4E3E034E7BC1EF4';

  if not Result then Exit;

  SetLength(Data, 42);
  Dig := BLAKE2SBytes(Data);

  Result := DataToHex(@Dig[0], SizeOf(TCnBLAKE2SDigest)) = 'D18DB82403F79BBAE8C650F76573B4816BB9F31C0EEB5BB55FC4DA72048AF1E5';

  if not Result then Exit;

  Dig := BLAKE2SBytes(Data, nil, 28);
  Result := DataToHex(@Dig[0], 28) = 'EA5117F70077EAC51FB3FD2BFFE4536C6B006003A77AEE393D6403ED';
end;

function TestBLAKE2B: Boolean;
var
  Dig: TCnBLAKE2BDigest;
  Data: TBytes;
begin
  Dig := BLAKE2BBytes(nil);
  Result := DataToHex(@Dig[0], SizeOf(TCnBLAKE2BDigest)) = '786A02F742015903C6C6FD852552D272912F4740E15847618A86E217F71F5419D25E1031AFEE585313896444934EB04B903A685B1448B755D56F701AFE9BE2CE';

  if not Result then Exit;

  Dig := BLAKE2BBytes(nil, nil, 48);
  Result := DataToHex(@Dig[0], 48) = 'B32811423377F52D7862286EE1A72EE540524380FDA1724A6F25D7978C6FD3244A6CAF0498812673C5E05EF583825100';

  if not Result then Exit;

  SetLength(Data, 88);
  Dig := BLAKE2BBytes(Data);

  Result := DataToHex(@Dig[0], SizeOf(TCnBLAKE2BDigest)) = '220B0F4F83FF1574197C964FD5EF5A6988F6A9A51A9E532ECEA32D89BB639A82450481151D4420E0E1BF9074B9D3E70AC5FCF041B63620A5B68B3998A1CE8B32';

  if not Result then Exit;

  Dig := BLAKE2BBytes(Data, nil, 48);
  Result := DataToHex(@Dig[0], 48) = 'AF0BAEF656E1C01ED54AD1963C7F473B52BD6DAE4BDDD87C419EC12F48BF6A06A3C09B94A260B8EA7B6C7B45E0071A99';
end;

function TestBLAKE2SKey: Boolean;
var
  Dig: TCnBLAKE2SDigest;
  Data, Key: TBytes;
begin
  Key := HexToBytes('000102030405060708090A0B0C0D0E0F101112131415161718191A1B1C1D1E1F');
  Dig := BLAKE2SBytes(nil, Key);
  Result := DataToHex(@Dig[0], SizeOf(TCnBLAKE2SDigest)) = '48A8997DA407876B3D79C0D92325AD3B89CBB754D86AB71AEE047AD345FD2C49';
end;

function TestBLAKE2BKey: Boolean;
var
  Dig: TCnBLAKE2BDigest;
  Data, Key: TBytes;
begin
  Key := HexToBytes('000102030405060708090A0B0C0D0E0F101112131415161718191A1B1C1D1E1F202122232425262728292A2B2C2D2E2F303132333435363738393A3B3C3D3E3F');
  Dig := BLAKE2BBytes(nil, Key);
  Result := DataToHex(@Dig[0], SizeOf(TCnBLAKE2BDigest)) = '10EBB67700B1868EFB4417987ACF4690AE9D972FB7A590C2F02871799AAA4786B5E996E8F0F4EB981FC214B005F42D2FF4233499391653DF7AEFCBC13FC51568';
end;

function TestBLAKE2SUpdate: Boolean;
var
  D1, D2: TCnBLAKE2SDigest;
  C: TCnBLAKE2SContext;
  S, S1, S2: AnsiString;
begin
  S1 := '0123456789abcdefghi';
  S2 := 'jklmnop';
  S := S1 + S2;

  D1 := BLAKE2SStringA(S);
  BLAKE2SInit(C);
  BLAKE2SUpdate(C, PAnsiChar(S1), Length(S1));
  BLAKE2SUpdate(C, PAnsiChar(S2), Length(S2));
  BLAKE2SFinal(C, D2);

  Result := BLAKE2SMatch(D1, D2);
end;

function TestBLAKE2BUpdate: Boolean;
var
  D1, D2: TCnBLAKE2BDigest;
  C: TCnBLAKE2BContext;
  S, S1, S2: AnsiString;
begin
  S1 := '0123456789abcdefghi';
  S2 := 'jklmnop';
  S := S1 + S2;

  D1 := BLAKE2BStringA(S);
  BLAKE2BInit(C);
  BLAKE2BUpdate(C, PAnsiChar(S1), Length(S1));
  BLAKE2BUpdate(C, PAnsiChar(S2), Length(S2));
  BLAKE2BFinal(C, D2);

  Result := BLAKE2BMatch(D1, D2);
end;

// ================================ Base64 =====================================

function TestBase64: Boolean;
var
  Res: string;
  Data, Output: TBytes;
begin
  Data := HexToBytes('000102030405060708090A0B0C0D0E0F32333425');
  if ECN_BASE64_OK = Base64Encode(Data, Res) then
    Result := Res = 'AAECAwQFBgcICQoLDA0ODzIzNCU='
  else
    Result := False;

  if not Result then Exit;

  if ECN_BASE64_OK = Base64Decode(Res, Output) then
    Result := CompareBytes(Data, Output)
  else
    Result := False;
end;

function TestBase64URL: Boolean;
var
  Res: string;
  Data, Output: TBytes;
begin
  Data := HexToBytes('7138482280EFC1DB9E401E3AF0AE710DCE7ADF7B1E105A2AC318C5FF1489C904');
  if ECN_BASE64_OK = Base64Encode(Data, Res, True) then
    Result := Res = 'cThIIoDvwdueQB468K5xDc5633seEFoqwxjF_xSJyQQ'
  else
    Result := False;

  if not Result then Exit;

  if ECN_BASE64_OK = Base64Decode(Res, Output) then
    Result := CompareBytes(Data, Output)
  else
    Result := False;
end;

// ================================ AEAD =======================================

function TestAEADAESCCM: Boolean;
var
  Key, Nonce, AAD, P, C, R: TBytes;
  T: TCnCCM128Tag;
begin
  // RFC 例子。注意须保证 CnAEAD 头部声明中的 Tag 8 字节，长 2 字节，也就是 CCM_M_LEN = 8; CCM_L_LEN = 2;
  Key := HexToBytes('C0C1C2C3C4C5C6C7C8C9CACBCCCDCECF');
  Nonce := HexToBytes('00000003020100A0A1A2A3A4A5');
  P := HexToBytes('08090A0B0C0D0E0F101112131415161718191A1B1C1D1E');
  AAD := HexToBytes('0001020304050607');

  C := AES128CCMEncryptBytes(Key, Nonce, P, AAD, T);
  Result := (DataToHex(@T[0], SizeOf(T)) = '17E8D12CFDF926E0') and
    (DataToHex(@C[0], Length(C)) = '588C979A61C663D2F066D0C2C0F989806D5F6B61DAC384');
  if not Result then Exit;

  R := AES128CCMDecryptBytes(Key, Nonce, C, AAD, T);
  Result := DataToHex(@R[0], Length(R)) = '08090A0B0C0D0E0F101112131415161718191A1B1C1D1E';
end;

function TestAEADSM4CCM: Boolean;
var
  Key, Nonce, AAD, P, C, R: TBytes;
  T: TCnCCM128Tag;
begin
  // 未按 RFC 标准实现，因为需要修改 CnAEAD 中的常量定义为 Tag 16 字节，长 3 字节
  Key := HexToBytes('0123456789ABCDEFFEDCBA9876543210');
  Nonce := HexToBytes('00001234567800000000ABCD');
  AAD := HexToBytes('FEEDFACEDEADBEEFFEEDFACEDEADBEEFABADDAD2');
  P := HexToBytes('AAAAAAAAAAAAAAAABBBBBBBBBBBBBBBBCCCCCCCCCCCCCCCCDDDDDDDDDDDDDDDDEEEEEEEEEEEEEEEEFFFFFFFFFFFFFFFFEEEEEEEEEEEEEEEEAAAAAAAAAAAAAAAA');
  C := SM4CCMEncryptBytes(Key, Nonce, P, AAD, T);
  Result := (DataToHex(@C[0], Length(C)) = '794758F9EEA0EA7BA8F8FE055EB786901AAFDD76EEF3C4CBDA26BB9DF9BE91589F33DEF61EE9C21204487153E313FB577A2053819853185E4E46C7F77A0ED1DA')
    and (DataToHex(@T[0], SizeOf(T)) = '5449D18B576EE743');
  if not Result then Exit;

  R := SM4CCMDecryptBytes(Key, Nonce, C, AAD, T);
  Result := DataToHex(@R[0], Length(R)) = 'AAAAAAAAAAAAAAAABBBBBBBBBBBBBBBBCCCCCCCCCCCCCCCCDDDDDDDDDDDDDDDDEEEEEEEEEEEEEEEEFFFFFFFFFFFFFFFFEEEEEEEEEEEEEEEEAAAAAAAAAAAAAAAA';
end;

function TestAEADAES128GCM: Boolean;
var
  Key, Iv, AD, Plain, C, P: TBytes;
  T: TCnGCM128Tag;
begin
  Key := HexToBytes('00000000000000000000000000000000');
  Iv := HexToBytes('000000000000000000000000');
  Plain := nil;
  AD := nil;

  C := AES128GCMEncryptBytes(Key, Iv, Plain, AD, T);  // Key Iv 全 0，Plain 和 AD 空，密文空
  Result := DataToHex(@T[0], SizeOf(T)) = '58E2FCCEFA7E3061367F1D57A4E7455A';
  if not Result then Exit;

  Key := HexToBytes('00000000000000000000000000000000');
  Iv := HexToBytes('000000000000000000000000');
  Plain := HexToBytes('00000000000000000000000000000000');
  AD := nil;

  C := AES128GCMEncryptBytes(Key, Iv, Plain, AD, T);  // Key Iv Plain 全 0，AD 空
  Result := (DataToHex(@C[0], Length(C)) = '0388DACE60B6A392F328C2B971B2FE78')
    and (DataToHex(@T[0], SizeOf(T)) = 'AB6E47D42CEC13BDF53A67B21257BDDF');
  if not Result then Exit;

  Key := HexToBytes('FEFFE9928665731C6D6A8F9467308308');
  Iv := HexToBytes('CAFEBABEFACEDBAD');
  Plain := HexToBytes('D9313225F88406E5A55909C5AFF5269A86A7A9531534F7DA2E4C303D8A318A721C3C0C95956809532FCF0E2449A6B525B16AEDF5AA0DE657BA637B39');
  AD := HexToBytes('FEEDFACEDEADBEEFFEEDFACEDEADBEEFABADDAD2');

  C := AES128GCMEncryptBytes(Key, Iv, Plain, AD, T);  // Key Iv Plain AD 全有，且 AD 非 96
  Result := (DataToHex(@C[0], Length(C)) = '61353B4C2806934A777FF51FA22A4755699B2A714FCDC6F83766E5F97B6C742373806900E49F24B22B097544D4896B424989B5E1EBAC0F07C23F4598')
    and (DataToHex(@T[0], SizeOf(T)) = '3612D2E79E3B0785561BE14AACA2FCCB');
  if not Result then Exit;

  // 解密
  Key := HexToBytes('FEFFE9928665731C6D6A8F9467308308');
  Iv := HexToBytes('CAFEBABEFACEDBAD');
  C := HexToBytes('61353B4C2806934A777FF51FA22A4755699B2A714FCDC6F83766E5F97B6C742373806900E49F24B22B097544D4896B424989B5E1EBAC0F07C23F4598');
  AD := HexToBytes('FEEDFACEDEADBEEFFEEDFACEDEADBEEFABADDAD2');
  HexToData('3612D2E79E3B0785561BE14AACA2FCCB', @T[0]);

  P := AES128GCMDecryptBytes(Key, Iv, C, AD, T);
  Result := DataToHex(@P[0], Length(P)) = 'D9313225F88406E5A55909C5AFF5269A86A7A9531534F7DA2E4C303D8A318A721C3C0C95956809532FCF0E2449A6B525B16AEDF5AA0DE657BA637B39';
end;

function TestAEADAES192GCM: Boolean;
var
  Key, Iv, AD, Plain, C, P: TBytes;
  T: TCnGCM128Tag;
begin
  Key := HexToBytes('000000000000000000000000000000000000000000000000');
  Iv := HexToBytes('000000000000000000000000');
  Plain := nil;
  AD := nil;

  C := AES192GCMEncryptBytes(Key, Iv, Plain, AD, T);  // Key Iv 全 0，Plain 和 AD 空，密文空
  Result := DataToHex(@T[0], SizeOf(T)) = 'CD33B28AC773F74BA00ED1F312572435';
  if not Result then Exit;

  Key := HexToBytes('000000000000000000000000000000000000000000000000');
  Iv := HexToBytes('000000000000000000000000');
  Plain := HexToBytes('00000000000000000000000000000000');
  AD := nil;

  C := AES192GCMEncryptBytes(Key, Iv, Plain, AD, T);  // Key Iv Plain 全 0，AD 空
  Result := (DataToHex(@C[0], Length(C)) = '98E7247C07F0FE411C267E4384B0F600') and
    (DataToHex(@T[0], SizeOf(T)) = '2FF58D80033927AB8EF4D4587514F0FB');
  if not Result then Exit;

  Key := HexToBytes('FEFFE9928665731C6D6A8F9467308308FEFFE9928665731C');
  Iv := HexToBytes('9313225DF88406E555909C5AFF5269AA6A7A9538534F7DA1E4C303D2A318A728C3C0C95156809539FCF0E2429A6B525416AEDBF5A0DE6A57A637B39B');
  Plain := HexToBytes('D9313225F88406E5A55909C5AFF5269A86A7A9531534F7DA2E4C303D8A318A721C3C0C95956809532FCF0E2449A6B525B16AEDF5AA0DE657BA637B39');
  AD := HexToBytes('FEEDFACEDEADBEEFFEEDFACEDEADBEEFABADDAD2');

  C := AES192GCMEncryptBytes(Key, Iv, Plain, AD, T);  // Key Iv Plain AD 全有，且 AD 非 96
  Result := (DataToHex(@C[0], Length(C)) = 'D27E88681CE3243C4830165A8FDCF9FF1DE9A1D8E6B447EF6EF7B79828666E4581E79012AF34DDD9E2F037589B292DB3E67C036745FA22E7E9B7373B')
    and (DataToHex(@T[0], SizeOf(T)) = 'DCF566FF291C25BBB8568FC3D376A6D9');
  if not Result then Exit;

  Key := HexToBytes('FEFFE9928665731C6D6A8F9467308308FEFFE9928665731C');
  Iv := HexToBytes('9313225DF88406E555909C5AFF5269AA6A7A9538534F7DA1E4C303D2A318A728C3C0C95156809539FCF0E2429A6B525416AEDBF5A0DE6A57A637B39B');
  C := HexToBytes('D27E88681CE3243C4830165A8FDCF9FF1DE9A1D8E6B447EF6EF7B79828666E4581E79012AF34DDD9E2F037589B292DB3E67C036745FA22E7E9B7373B');
  AD := HexToBytes('FEEDFACEDEADBEEFFEEDFACEDEADBEEFABADDAD2');

  HexToData('DCF566FF291C25BBB8568FC3D376A6D9', @T[0]);

  P := AES192GCMDecryptBytes(Key, Iv, C, AD, T);
  Result := DataToHex(@P[0], Length(P)) = 'D9313225F88406E5A55909C5AFF5269A86A7A9531534F7DA2E4C303D8A318A721C3C0C95956809532FCF0E2449A6B525B16AEDF5AA0DE657BA637B39';
end;

function TestAEADAES256GCM: Boolean;
var
  Key, Iv, AD, Plain, C, P: TBytes;
  T: TCnGCM128Tag;
begin
  Key := HexToBytes('0000000000000000000000000000000000000000000000000000000000000000');
  Iv := HexToBytes('000000000000000000000000');
  Plain := nil;
  AD := nil;

  C := AES256GCMEncryptBytes(Key, Iv, Plain, AD, T);  // Key Iv 全 0，Plain 和 AD 空，密文空
  Result := DataToHex(@T[0], SizeOf(T)) = '530F8AFBC74536B9A963B4F1C4CB738B';
  if not Result then Exit;

  Key := HexToBytes('0000000000000000000000000000000000000000000000000000000000000000');
  Iv := HexToBytes('000000000000000000000000');
  Plain := HexToBytes('00000000000000000000000000000000');
  AD := nil;

  C := AES256GCMEncryptBytes(Key, Iv, Plain, AD, T);  // Key Iv Plain 全 0，AD 空
  Result := (DataToHex(@C[0], Length(C)) = 'CEA7403D4D606B6E074EC5D3BAF39D18')
    and (DataToHex(@T[0], SizeOf(T)) = 'D0D1C8A799996BF0265B98B5D48AB919');
  if not Result then Exit;

  Key := HexToBytes('FEFFE9928665731C6D6A8F9467308308FEFFE9928665731C6D6A8F9467308308');
  Iv := HexToBytes('CAFEBABEFACEDBADDECAF888');
  Plain := HexToBytes('D9313225F88406E5A55909C5AFF5269A86A7A9531534F7DA2E4C303D8A318A721C3C0C95956809532FCF0E2449A6B525B16AEDF5AA0DE657BA637B39');
  AD := HexToBytes('FEEDFACEDEADBEEFFEEDFACEDEADBEEFABADDAD2');

  C := AES256GCMEncryptBytes(Key, Iv, Plain, AD, T);  // Key Iv Plain AD 全有，且 AD 非 96
  Result := (DataToHex(@C[0], Length(C)) = '522DC1F099567D07F47F37A32A84427D643A8CDCBFE5C0C97598A2BD2555D1AA8CB08E48590DBB3DA7B08B1056828838C5F61E6393BA7A0ABCC9F662')
    and (DataToHex(@T[0], SizeOf(T)) = '76FC6ECE0F4E1768CDDF8853BB2D551B');
  if not Result then Exit;

  Key := HexToBytes('FEFFE9928665731C6D6A8F9467308308FEFFE9928665731C6D6A8F9467308308');
  Iv := HexToBytes('CAFEBABEFACEDBADDECAF888');
  C := HexToBytes('522DC1F099567D07F47F37A32A84427D643A8CDCBFE5C0C97598A2BD2555D1AA8CB08E48590DBB3DA7B08B1056828838C5F61E6393BA7A0ABCC9F662');
  AD := HexToBytes('FEEDFACEDEADBEEFFEEDFACEDEADBEEFABADDAD2');

  HexToData('76FC6ECE0F4E1768CDDF8853BB2D551B', @T[0]);

  P := AES256GCMDecryptBytes(Key, Iv, C, AD, T);
  Result := DataToHex(@P[0], Length(P)) = 'D9313225F88406E5A55909C5AFF5269A86A7A9531534F7DA2E4C303D8A318A721C3C0C95956809532FCF0E2449A6B525B16AEDF5AA0DE657BA637B39';
end;

function TestAEADSM4GCM: Boolean;
var
  Key, Iv, AD, Plain, C, P: TBytes;
  T: TCnGCM128Tag;
begin
  Key := HexToBytes('0123456789ABCDEFFEDCBA9876543210');
  Iv := HexToBytes('00001234567800000000ABCD');
  Plain := HexToBytes('AAAAAAAAAAAAAAAABBBBBBBBBBBBBBBBCCCCCCCCCCCCCCCCDDDDDDDDDDDDDDDDEEEEEEEEEEEEEEEEFFFFFFFFFFFFFFFFEEEEEEEEEEEEEEEEAAAAAAAAAAAAAAAA');
  AD := HexToBytes('FEEDFACEDEADBEEFFEEDFACEDEADBEEFABADDAD2');

  C := SM4GCMEncryptBytes(Key, Iv, Plain, AD, T);  // 例子数据来源于 RFC 8998
  Result := (DataToHex(@C[0], Length(C)) = '17F399F08C67D5EE19D0DC9969C4BB7D5FD46FD3756489069157B282BB200735D82710CA5C22F0CCFA7CBF93D496AC15A56834CBCF98C397B4024A2691233B8D')
    and (DataToHex(@T[0], SizeOf(T)) = '83DE3541E4C2B58177E065A9BF7B62EC');
  if not Result then Exit;

  P := SM4GCMDecryptBytes(Key, Iv, C, AD, T);
  Result := DataToHex(@P[0], Length(P)) = 'AAAAAAAAAAAAAAAABBBBBBBBBBBBBBBBCCCCCCCCCCCCCCCCDDDDDDDDDDDDDDDDEEEEEEEEEEEEEEEEFFFFFFFFFFFFFFFFEEEEEEEEEEEEEEEEAAAAAAAAAAAAAAAA';
end;

function TestAEADChaCha20Poly1305: Boolean;
var
  Plain, Key, AAD, Iv, EnData, DeData: TBytes;
  Tag: TCnPoly1305Digest;
begin
  // 例子来自 RFC 8439
  Plain := AnsiToBytes('Ladies and Gentlemen of the class of ''99: If I could offer you only one tip for the future, sunscreen would be it.');
  AAD := HexToBytes('50515253C0C1C2C3C4C5C6C7');
  Key := HexToBytes('808182838485868788898A8B8C8D8E8F909192939495969798999A9B9C9D9E9F');
  Iv := HexToBytes('070000004041424344454647');

  EnData := ChaCha20Poly1305EncryptBytes(Key, Iv, Plain, AAD, Tag);

  Result := DataToHex(@Tag[0], SizeOf(TCnPoly1305Digest)) = '1AE10B594F09E26A7E902ECBD0600691';

  if not Result then Exit;
    Result := DataToHex(@EnData[0], Length(EnData)) =
      'D31A8D34648E60DB7B86AFBC53EF7EC2A4ADED51296E08FEA9E2B5A736EE62D6' +
      '3DBEA45E8CA9671282FAFB69DA92728B1A71DE0A9E060B2905D6A5B67ECD3B36' +
      '92DDBD7F2D778B8C9803AEE328091B58FAB324E4FAD675945585808B4831D7BC' +
      '3FF4DEF08E4B7A9DE576D26586CEC64B6116';

  if not Result then Exit;

  DeData := ChaCha20Poly1305DecryptBytes(Key, Iv, EnData, AAD, Tag);
  Result := CompareBytes(DeData, Plain);
end;


function TestAEADXChaCha20Poly1305: Boolean;
var
  Plain, Key, AAD, Iv, EnData, DeData: TBytes;
  Tag: TCnPoly1305Digest;
begin
  // 例子来自 RFC 草案
  Plain := AnsiToBytes('Ladies and Gentlemen of the class of ''99: If I could offer you only one tip for the future, sunscreen would be it.');
  AAD := HexToBytes('50515253C0C1C2C3C4C5C6C7');
  Key := HexToBytes('808182838485868788898A8B8C8D8E8F909192939495969798999A9B9C9D9E9F');
  Iv := HexToBytes('404142434445464748494a4b4c4d4e4f5051525354555657');

  EnData := XChaCha20Poly1305EncryptBytes(Key, Iv, Plain, AAD, Tag);

  Result := DataToHex(@Tag[0], SizeOf(TCnPoly1305Digest)) = 'C0875924C1C7987947DEAFD8780ACF49';

  if not Result then Exit;
    Result := DataToHex(@EnData[0], Length(EnData)) =
      'BD6D179D3E83D43B9576579493C0E939572A1700252BFACCBED2902C21396CBB' +
      '731C7F1B0B4AA6440BF3A82F4EDA7E39AE64C6708C54C216CB96B72E1213B452' +
      '2F8C9BA40DB5D945B11B69B982C1BB9E3F3FAC2BC369488F76B2383565D3FFF9' +
      '21F9664C97637DA9768812F615C68B13B52E';

  if not Result then Exit;

  DeData := XChaCha20Poly1305DecryptBytes(Key, Iv, EnData, AAD, Tag);
  Result := CompareBytes(DeData, Plain);
end;

// ================================ ChaCha20 ===================================

function TestChaCha20: Boolean;
var
  S: AnsiString;
  SKey, SNonce: string;
  Key: TCnChaChaKey;
  Nonce: TCnChaChaNonce;
  EnRes, DeRes: TBytes;
begin
  // 例子数据来源于 ChaCha20 的 RFC 8439
  SKey := '000102030405060708090A0B0C0D0E0F101112131415161718191A1B1C1D1E1F';
  SNonce := '000000000000004A00000000';

  HexToData(SKey, @Key[0]);
  HexToData(SNonce, @Nonce[0]);

  S := 'Ladies and Gentlemen of the class of ''99: If I could offer you only one tip for the future, sunscreen would be it.';
  SetLength(EnRes, Length(S));

  ChaCha20EncryptData(Key, Nonce, @S[1], Length(S), @EnRes[0]);
  Result := BytesToHex(EnRes) = '6E2E359A2568F98041BA0728DD0D6981E97E7AEC1D4360C20A27AFCCFD9FAE0BF91B65C5524733AB8F593DABCD62B3571639D624E65152AB8F530C359F0861D807CA0DBF500D6A6156A38E088A22B65E52BC514D16CCF806818CE91AB77937365AF90BBF74A35BE6B40B8EEDF2785E42874D';
  if not Result then Exit;

  DeRes := ChaCha20DecryptBytes(Key, Nonce, EnRes);
  Result := (DeRes <> nil) and CompareMem(@S[1], @DeRes[0], Length(DeRes));
end;

function TestHChaCha20SubKey: Boolean;
var
  SKey, SNonce: AnsiString;
  Key: TCnChaChaKey;
  Nonce: TCnHChaChaNonce;
  SubKey: TCnHChaChaSubKey;
begin
  // 例子数据来源于 XChaCha20 的 RFC 草案
  SKey := '000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f';
  SNonce := '000000090000004a0000000031415927';

  HexToData(SKey, @Key[0]);
  HexToData(SNonce, @Nonce[0]);

  HChaCha20SubKey(Key, Nonce, SubKey);

  Result := DataToHex(@SubKey[0], SizeOf(TCnHChaChaSubKey)) = '82413B4227B27BFED30E42508A877D73A0F9E4D58A74A853C12EC41326D3ECDC';
end;

function TestXChaCha20: Boolean;
var
  SKey, SNonce, Plain: AnsiString;
  Key: TCnChaChaKey;
  Nonce: TCnXChaChaNonce;
  PT, Res: TBytes;
begin
  // 例子数据来源于 XChaCha20 的 RFC 草案
  SKey := '808182838485868788898A8B8C8D8E8F909192939495969798999A9B9C9D9E9F';
  SNonce := '404142434445464748494A4B4C4D4E4F5051525354555658';
  Plain :=
    '5468652064686F6C65202870726F6E6F756E6365642022646F6C652229206973' +
    '20616C736F206B6E6F776E2061732074686520417369617469632077696C6420' +
    '646F672C2072656420646F672C20616E642077686973746C696E6720646F672E' +
    '2049742069732061626F7574207468652073697A65206F662061204765726D61' +
    '6E20736865706865726420627574206C6F6F6B73206D6F7265206C696B652061' +
    '206C6F6E672D6C656767656420666F782E205468697320686967686C7920656C' +
    '757369766520616E6420736B696C6C6564206A756D70657220697320636C6173' +
    '736966696564207769746820776F6C7665732C20636F796F7465732C206A6163' +
    '6B616C732C20616E6420666F78657320696E20746865207461786F6E6F6D6963' +
    '2066616D696C792043616E696461652E';

  HexToData(SKey, @Key[0]);
  HexToData(SNonce, @Nonce[0]);
  PT := HexToBytes(Plain);

  Res := XChaCha20EncryptBytes(Key, Nonce, PT);
  Result := DataToHex(@Res[0], Length(Res)) =
    '7D0A2E6B7F7C65A236542630294E063B7AB9B555A5D5149AA21E4AE1E4FBCE87' +
    'ECC8E08A8B5E350ABE622B2FFA617B202CFAD72032A3037E76FFDCDC4376EE05' +
    '3A190D7E46CA1DE04144850381B9CB29F051915386B8A710B8AC4D027B8B050F' +
    '7CBA5854E028D564E453B8A968824173FC16488B8970CAC828F11AE53CABD201' +
    '12F87107DF24EE6183D2274FE4C8B1485534EF2C5FBC1EC24BFC3663EFAA08BC' +
    '047D29D25043532DB8391A8A3D776BF4372A6955827CCB0CDD4AF403A7CE4C63' +
    'D595C75A43E045F0CCE1F29C8B93BD65AFC5974922F214A40B7C402CDB91AE73' +
    'C0B63615CDAD0480680F16515A7ACE9D39236464328A37743FFC28F4DDB324F4' +
    'D0F5BBDC270C65B1749A6EFFF1FBAA09536175CCD29FB9E6057B307320D31683' +
    '8A9C71F70B5B5907A66F7EA49AADC409';
end;

// ================================ Poly1305 ===================================

function TestPoly1305: Boolean;
var
  S: AnsiString;
  Key: TCnPoly1305Key;
  Dig: TCnPoly1305Digest;
begin
  S := 'Cryptographic Forum Research Group';
  HexToData('85D6BE7857556D337F4452FE42D506A80103808AFB0DB2FD4ABFF6AF4149F51B', @Key[0]);
  Dig := Poly1305Data(@S[1], Length(S), Key);
  Result := DataToHex(@Dig[0], SizeOf(TCnPoly1305Digest)) = 'A8061DC1305136C6C22B8BAF0C0127A9';
end;

function TestPoly1305Update: Boolean;
var
  D1, D2: TCnPoly1305Digest;
  C: TCnPoly1305Context;
  K: TCnPoly1305Key;
  S, S1, S2: AnsiString;
begin
  S1 := '0123456789abcdefghi';
  S2 := 'jklmnop';
  S := S1 + S2;

  FillChar(K[0], SizeOf(TCnPoly1305Key), $FF);

  D1 := Poly1305Buffer(S, Length(S), K);
  Poly1305Init(C, K);
  Poly1305Update(C, PAnsiChar(S1), Length(S1));
  Poly1305Update(C, PAnsiChar(S2), Length(S2));
  Poly1305Final(C, D2);

  Result := Poly1305Match(D1, D2);
end;

// ================================ ZUC ========================================

function TestZUC1: Boolean;
var
  Key, IV: array[0..15] of Byte;
  KeyStream: array[0..1] of Cardinal;
begin
  FillChar(Key[0], SizeOf(Key), 0);
  FillChar(IV[0], SizeOf(IV), 0);
  ZUC(@Key[0], @IV[0], PCardinal(@KeyStream[0]), SizeOf(KeyStream) div SizeOf(Cardinal));

  Result := (KeyStream[0] = $27BEDE74) and (KeyStream[1] = $018082DA);
end;

function TestZUC2: Boolean;
var
  Key, IV: array[0..15] of Byte;
  KeyStream: array[0..1] of Cardinal;
begin
  FillChar(Key[0], SizeOf(Key), $FF);
  FillChar(IV[0], SizeOf(IV), $FF);
  ZUC(@Key[0], @IV[0], PCardinal(@KeyStream[0]), SizeOf(KeyStream) div SizeOf(Cardinal));

  Result := (KeyStream[0] = $0657CFA0) and (KeyStream[1] = $7096398B);
end;

function TestZUC3: Boolean;
const
  Key: array[0..15] of Byte = ($3D, $4C, $4B, $E9, $6A, $82, $FD, $AE, $B5, $8F,
    $64, $1D, $B1, $7B, $45, $5B);
  IV: array[0..15] of Byte = ($84, $31, $9A, $A8, $DE, $69, $15, $CA, $1F, $6B,
    $DA, $6B, $FB, $D8, $C7, $66);
var
  KeyStream: array[0..1] of Cardinal;
begin
  ZUC(@Key[0], @IV[0], PCardinal(@KeyStream[0]), SizeOf(KeyStream) div SizeOf(Cardinal));
  Result := (KeyStream[0] = $14F1C272) and (KeyStream[1] = $3279C419);
end;

function TestZUC4: Boolean;
const
  Key: array[0..15] of Byte = ($4D, $32, $0B, $FA, $D4, $C2, $85, $BF, $D6, $B8,
    $BD, $00, $F3, $9D, $8B, $41);
  IV: array[0..15] of Byte = ($52, $95, $9D, $AB, $A0, $BF, $17, $6E, $CE, $2D,
    $C3, $15, $04, $9E, $B5, $74);
var
  KeyStream: array[0..1999] of Cardinal;
begin
  ZUC(@Key[0], @IV[0], PCardinal(@KeyStream[0]), SizeOf(KeyStream) div SizeOf(Cardinal));
  Result := (KeyStream[0] = $ED4400E7) and (KeyStream[1] = $0633E5C5) and (KeyStream[1999] = $7A574CDB);
end;

function TestZUCEEA31: Boolean;
const
  Key: array[0..15] of Byte = ($17, $3D, $14, $BA, $50, $03, $73, $1D, $7A, $60,
    $04, $94, $70, $F0, $0A, $29);
  Plain: array[0..6] of Cardinal = ($6CF65340, $735552AB, $0C9752FA, $6F9025FE,
    $0BD675D9, $005875B2, 0);
var
  Cipher: array[0..6] of Cardinal;
begin
  FillChar(Cipher[0], SizeOf(Cipher), 0);
  ZUCEEA3(@Key[0], $66035492, $F, 0, @Plain[0], 193, @Cipher[0]);

  Result := (Cipher[0] = $A6C85FC6) and (Cipher[1] = $6AFB8533) and (Cipher[2] = $AAFC2518)
    and (Cipher[3] = $DFE78494) and (Cipher[4] = $0EE1E4B0) and (Cipher[5] = $30238CC8) and (Cipher[6] = 0);
  if not Result then Exit;

  ZUCEEA3(@Key[0], $66035492, $F, 0, @Cipher[0], 193, @Cipher[0]);
  Result := CompareMem(@Cipher[0], @Plain[0], SizeOf(Cipher));
end;

function TestZUCEEA32: Boolean;
const
  Key: array[0..15] of Byte = ($E5, $BD, $3E, $A0, $EB, $55, $AD, $E8, $66, $C6, $AC, $58, $BD, $54, $30, $2A);
  Plain: array[0..24] of Cardinal = ($14A8EF69, $3D678507, $BBE7270A, $7F67FF50, $06C3525B, $9807E467, $C4E56000, $BA338F5D,
    $42955903, $67518222, $46C80D3B, $38F07F4B, $E2D8FF58, $05F51322, $29BDE93B, $BBDCAF38,
    $2BF1EE97, $2FBF9977, $BADA8945, $847A2A6C, $9AD34A66, $7554E04D, $1F7FA2C3, $3241BD8F,
    $01BA220D);
  Res: array[0..24] of Cardinal = ($131D43E0, $DEA1BE5C, $5A1BFD97, $1D852CBF, $712D7B4F, $57961FEA, $3208AFA8, $BCA433F4,
    $56AD09C7, $417E58BC, $69CF8866, $D1353F74, $865E8078, $1D202DFB, $3ECFF7FC, $BC3B190F,
    $E82A204E, $D0E350FC, $0F6F2613, $B2F2BCA6, $DF5A473A, $57A4A00D, $985EBAD8, $80D6F238,
    $64A07B01);
var
  Cipher: array[0..24] of Cardinal;
begin
  FillChar(Cipher[0], SizeOf(Cipher), 0);
  ZUCEEA3(@Key[0], $56823, $18, 1, @Plain[0], 800, @Cipher[0]);

  Result := CompareMem(@Cipher[0], @Res[0], SizeOf(Cipher));
  if not Result then Exit;

  ZUCEEA3(@Key[0], $56823, $18, 1, @Cipher[0], 800, @Cipher[0]);
  Result := CompareMem(@Cipher[0], @Plain[0], SizeOf(Cipher));
end;

function TestZUCEEA33: Boolean;
const
  Key: array[0..15] of Byte = ($DB, $84, $B4, $FB, $CC, $DA, $56, $3B, $66, $22, $7B, $FE, $45, $6F, $0F, $77);
  Plain: array[0..87] of Cardinal = ($E539F3B8, $973240DA, $03F2B8AA, $05EE0A00, $DBAFC0E1, $82055DFE, $3D7383D9, $2CEF40E9,
    $2928605D, $52D05F4F, $9018A1F1, $89AE3997, $CE19155F, $B1221DB8, $BB0951A8, $53AD852C,
    $E16CFF07, $382C93A1, $57DE00DD, $B125C753, $9FD85045, $E4EE07E0, $C43F9E9D, $6F414FC4,
    $D1C62917, $813F74C0, $0FC83F3E, $2ED7C45B, $A5835264, $B43E0B20, $AFDA6B30, $53BFB642,
    $3B7FCE25, $479FF5F1, $39DD9B5B, $995558E2, $A56BE18D, $D581CD01, $7C735E6F, $0D0D97C4,
    $DDC1D1DA, $70C6DB4A, $12CC9277, $8E2FBBD6, $F3BA52AF, $91C9C6B6, $4E8DA4F7, $A2C266D0,
    $2D001753, $DF089603, $93C5D568, $88BF49EB, $5C16D9A8, $0427A416, $BCB597DF, $5BFE6F13,
    $890A07EE, $1340E647, $6B0D9AA8, $F822AB0F, $D1AB0D20, $4F40B7CE, $6F2E136E, $B67485E5,
    $07804D50, $4588AD37, $FFD81656, $8B2DC403, $11DFB654, $CDEAD47E, $2385C343, $6203DD83,
    $6F9C64D9, $7462AD5D, $FA63B5CF, $E08ACB95, $32866F5C, $A787566F, $CA93E6B1, $693EE15C,
    $F6F7A2D6, $89D97417, $98DC1C23, $8E1BE650, $733B18FB, $34FF880E, $16BBD21B, $47AC0000);
  Res: array[0..87] of Cardinal = ($4BBFA91B, $A25D47DB, $9A9F190D, $962A19AB, $323926B3, $51FBD39E, $351E05DA, $8B8925E3,
    $0B1CCE0D, $12211010, $95815CC7, $CB631950, $9EC0D679, $40491987, $E13F0AFF, $AC332AA6,
    $AA64626D, $3E9A1917, $519E0B97, $B655C6A1, $65E44CA9, $FEAC0790, $D2A321AD, $3D86B79C,
    $5138739F, $A38D887E, $C7DEF449, $CE8ABDD3, $E7F8DC4C, $A9E7B733, $14AD310F, $9025E619,
    $46B3A56D, $C649EC0D, $A0D63943, $DFF592CF, $962A7EFB, $2C8524E3, $5A2A6E78, $79D62604,
    $EF268695, $FA400302, $7E22E608, $30775220, $64BD4A5B, $906B5F53, $1274F235, $ED506CFF,
    $0154C754, $928A0CE5, $476F2CB1, $020A1222, $D32C1455, $ECAEF1E3, $68FB344D, $1735BFBE,
    $DEB71D0A, $33A2A54B, $1DA5A294, $E679144D, $DF11EB1A, $3DE8CF0C, $C0619179, $74F35C1D,
    $9CA0AC81, $807F8FCC, $E6199A6C, $7712DA86, $5021B04C, $E0439516, $F1A526CC, $DA9FD9AB,
    $BD53C3A6, $84F9AE1E, $7EE6B11D, $A138EA82, $6C5516B5, $AADF1ABB, $E36FA7FF, $F92E3A11,
    $76064E8D, $95F2E488, $2B5500B9, $3228B219, $4A475C1A, $27F63F9F, $FD264989, $A1BC0000);
var
  Cipher: array[0..87] of Cardinal;
begin
  FillChar(Cipher[0], SizeOf(Cipher), 0);
  ZUCEEA3(@Key[0], $E4850FE1, $10, 1, @Plain[0], 2798, @Cipher[0]);

  Result := CompareMem(@Cipher[0], @Res[0], SizeOf(Cipher));
  if not Result then Exit;

  ZUCEEA3(@Key[0], $E4850FE1, $10, 1, @Cipher[0], 2798, @Cipher[0]);
  Result := CompareMem(@Cipher[0], @Plain[0], SizeOf(Cipher));
end;

function TestZUCEIA31: Boolean;
var
  Key: array[0..15] of Byte;
  Msg: Cardinal;
  Mac: Cardinal;
begin
  FillChar(Key[0], SizeOf(Key), 0);
  Msg := 0;
  ZUCEIA3(@Key[0], 0, 0, 0, @Msg, 1, Mac);
  Result := Mac = $C8A9595E;
end;

function TestZUCEIA32: Boolean;
const
  Key: array[0..15] of Byte = ($47, $05, $41, $25, $56, $1E, $B2, $DD, $A9, $40, $59, $DA, $05, $09, $78, $50);
var
  Msg: array[0..2] of Cardinal;
  Mac: Cardinal;
begin
  FillChar(Msg[0], SizeOf(Msg), 0);
  ZUCEIA3(@Key[0], $561EB2DD, $14, 0, @Msg[0], 90, Mac);
  Result := Mac = $6719A088;
end;

function TestZUCEIA33: Boolean;
const
  Key: array[0..15] of Byte = ($C9, $E6, $CE, $C4, $60, $7C, $72, $DB, $00, $0A, $EF, $A8, $83, $85, $AB, $0A);
  Msg: array[0..18] of Cardinal = ($983B41D4, $7D780C9E, $1AD11D7E, $B70391B1, $DE0B35DA, $2DC62F83, $E7B78D63, $06CA0EA0,
    $7E941B7B, $E91348F9, $FCB170E2, $217FECD9, $7F9F68AD, $B16E5D7D, $21E569D2, $80ED775C, $EBDE3F40, $93C53881, 0);
var
  Mac: Cardinal;
begin
  ZUCEIA3(@Key[0], $A94059DA, $A, 1, @Msg[0], 577, Mac);
  Result := Mac = $FAE8FF0B;
end;

// ================================ RC4 ========================================

function TestRC4: Boolean;
var
  S, K, D: AnsiString;
begin
  S := 'Sample Text';
  K := '123456';

  D := RC4EncryptStrToHex(S, K);
  Result := D = '53991317485635C81A4F56';

  if not Result then Exit;

  D := RC4DecryptStrFromHex(D, K);

  Result := D = S;
end;

// ================================ TEA ========================================

function TestTea: Boolean;
var
  TeaKey: TCnTeaKey;
  TeaData: TCnTeaData;
begin
  TeaKey[0] := $A0B1C2D3;
  TeaKey[1] := $E4F5A6B7;
  TeaKey[2] := $C8D9EAFB;
  TeaKey[3] := $ACBDCEDF;
  TeaData[0] := $12345678;
  TeaData[1] := $9ABCDEF0;

  CnTeaEncrypt(TeaKey, TeaData);
  Result := (TeaData[0] = $6E47CFDB) and (TeaData[1] = $FBC61842);
  if not Result then Exit;

  CnTeaDecrypt(TeaKey, TeaData);
  Result := (TeaData[0] = $12345678) and (TeaData[1] = $9ABCDEF0);
end;

function TestXTea: Boolean;
var
  TeaKey: TCnTeaKey;
  TeaData: TCnTeaData;
begin
  TeaKey[0] := $A0B1C2D3;
  TeaKey[1] := $E4F5A6B7;
  TeaKey[2] := $C8D9EAFB;
  TeaKey[3] := $ACBDCEDF;
  TeaData[0] := $12345678;
  TeaData[1] := $9ABCDEF0;

  CnXTeaEncrypt(TeaKey, TeaData);
  Result := (TeaData[0] = $7C5C0473) and (TeaData[1] = $C5957C19);
  if not Result then Exit;

  CnXTeaDecrypt(TeaKey, TeaData);
  Result := (TeaData[0] = $12345678) and (TeaData[1] = $9ABCDEF0);
end;

function TestXXTea: Boolean;
var
  TeaKey: TCnTeaKey;
  TeaData: TCnTeaData;
begin
  TeaKey[0] := $A0B1C2D3;
  TeaKey[1] := $E4F5A6B7;
  TeaKey[2] := $C8D9EAFB;
  TeaKey[3] := $ACBDCEDF;
  TeaData[0] := $12345678;
  TeaData[1] := $9ABCDEF0;

  CnXXTeaEncrypt(TeaKey, @TeaData[0], SizeOf(TCnTeaData) div SizeOf(Cardinal));
  Result := (TeaData[0] = $3F4E62BB) and (TeaData[1] = $D187DE94);
  if not Result then Exit;

  CnXXTeaDecrypt(TeaKey, @TeaData[0], SizeOf(TCnTeaData) div SizeOf(Cardinal));
  Result := (TeaData[0] = $12345678) and (TeaData[1] = $9ABCDEF0);
end;

// ================================ FNV ========================================

function TestFNV1: Boolean;
var
  S: AnsiString;
  R32: TCnFNVHash32;
  R64: TCnFNVHash64;
  R128: TCnFNVHash128;
  R256: TCnFNVHash256;
  R512: TCnFNVHash512;
  R1024: TCnFNVHash1024;
begin
  S := 'CnPack Test';
  R32 := FNV1Hash32(@S[1], Length(S));
  R64 := FNV1Hash64(@S[1], Length(S));
  R128 := FNV1Hash128(@S[1], Length(S));
  R256 := FNV1Hash256(@S[1], Length(S));
  R512 := FNV1Hash512(@S[1], Length(S));
  R1024 := FNV1Hash1024(@S[1], Length(S));

  Result  := (DataToHex(@R32[0], SizeOf(TCnFNVHash32)) = '6C942797')
    and (DataToHex(@R64[0], SizeOf(TCnFNVHash64)) = 'ED78DF90BF4705F7')
    and (DataToHex(@R128[0], SizeOf(TCnFNVHash128)) = '9FDD06116E58550841478B690F1987DF')
    and (DataToHex(@R256[0], SizeOf(TCnFNVHash256)) = 'D41B9B05355E0B5605A530EC0883AAD4DB43EEA7B7BF7DC168E67C776B2BB1E7')
    and (DataToHex(@R512[0], SizeOf(TCnFNVHash512)) = '000093BF8B221FDB9305331987CA9405EE207CD80D000000000000000000000000000000000000000000000011FF0EDB280FED457327DA3AC36257CB3335312E')
    and (DataToHex(@R1024[0], SizeOf(TCnFNVHash1024)) = '3FA9D253E52AE80105B382C80A01E27A53D7BC1D201EFB47B38F4D6E465488F81C0F43E9072F908DBCA3A30000000000000000000000000000000000000000000'
      + '0000000000000000000000000000000000253EB20F42A7228AF9022D9F35ECE5BB71E40FCD8717B80D164AB921709996E5C4397605870150BFF1F2AA31D53D9');
end;

function TestFNV1a: Boolean;
var
  S: AnsiString;
  R32: TCnFNVHash32;
  R64: TCnFNVHash64;
  R128: TCnFNVHash128;
  R256: TCnFNVHash256;
  R512: TCnFNVHash512;
  R1024: TCnFNVHash1024;
begin
  S := 'CnPack Test';
  R32 := FNV1aHash32(@S[1], Length(S));
  R64 := FNV1aHash64(@S[1], Length(S));
  R128 := FNV1aHash128(@S[1], Length(S));
  R256 := FNV1aHash256(@S[1], Length(S));
  R512 := FNV1aHash512(@S[1], Length(S));
  R1024 := FNV1aHash1024(@S[1], Length(S));

  Result := (DataToHex(@R32[0], SizeOf(TCnFNVHash32)) = '70BC2FDB')
    and (DataToHex(@R64[0], SizeOf(TCnFNVHash64)) = '0B2F2A33D0684C7B')
    and (DataToHex(@R128[0], SizeOf(TCnFNVHash128)) = 'ACE1FE5B039B0404E1E97664DCAFA2D3')
    and (DataToHex(@R256[0], SizeOf(TCnFNVHash256)) = '9612F703060D51E09F686AEC0883AAD4DB43EEB1051F744B884B4FB9DAC7314B')
    and (DataToHex(@R512[0], SizeOf(TCnFNVHash512)) =   '000093BF8B221FDB9337A00C1232AFFB766F227EF3000000000000000000000000000000000000000000000011FF0EDB280FED4B9760B10B4FA20363B8261786')
    and (DataToHex(@R1024[0], SizeOf(TCnFNVHash1024)) = '3FA9D253E52AE80105B382C80A01E27A53D7BC1D201EFB47B38F4D6E465489CAD8F2E23BEDE6954C0B8699000000000000000000000000000000000000000000'
      + '00000000000000000000000000000000000253EB20F42A7228AF9022D9F35ECE5BB71E40FCD8717B80D164AB921709996E5C43B515A262332A46CD9B163889E1');
end;

// ================================ FEC ========================================

function TestHamming: Boolean;
var
  I: Integer;
  IB, OB: TBits;
  SI, SO: string;
begin
  IB := nil;
  OB := nil;

  try
    IB := TBits.Create;
    IB.Size := 128;

    // 原始数据
    for I := 0 to IB.Size - 1 do
      IB.Bits[I] := I mod 2 = 0;

    // 根据 IB 计算校验码
    OB := TBits.Create;
    CnCalcHammingCode(IB, OB, 8);

    SO := BitsToString(OB);
    Result := SO = '111101001010111101001010111101001010111101001010111101001010111101001010111101001010111101001010111101001010111101001010111101001010111101001010111101001010111101001010111101001010111101001010';
    if not Result then Exit;

    SI := BitsToString(IB);

    // 模拟传输混乱的个别位翻转
    IB[35] := not IB[35];
    IB[79] := not IB[79];

    // 再验证并纠错
    CnVerifyHammingCode(IB, OB, 8);
    Result := BitsToString(IB) = SI;
  finally
    OB.Free;
    IB.Free;
  end;
end;

// ================================ DSA ========================================

function TestDSA1: Boolean;
var
  Param: TCnDSADomainParameter;
begin
  Param := TCnDSADomainParameter.Create;
  // 1024-160
  Param.P.SetHex('95D8E3C9FC1E748F63C83EFD90EB7ED6871AF087F975FF64048028880C0365C505506FFB6EF74911F1164B24EDDACF2D07B14BA84E38A0AC39C4FB8A4'
    + 'C9B816EA36C2EE2CF4E276D7BBA5F6A76EC3447C7BC4EBD190575C54814FEDB84FC4EEA456921CC1E3FAAA4B96FFABC7A8C00E6427D47A032C5EBEBF0F86192BF25635B');
  Param.Q.SetHex('B18265035E348B7F9993893D99E9CECFC45AFA33');
  Param.G.SetHex('67636611F85C6706C1C53D33553050941A1B5399AA6EA6C9A398ACE01862E6AF491A2BC9B65B977756EAAE11CB3755CDC45905AACD10290A1BC1E99AF'
    + '819A9A9EC8C987F98171EADD4F952EAE538B312612EBD68A88054E6F6D0B5B4EA253A033636F56C007D4FA5454CC40EBEE1794B7B3DACC41878FFC899A457899DF95994');

  Result := CnDSAVerifyParameter(Param);
  Param.Free;
end;

function TestDSA2: Boolean;
var
  Param: TCnDSADomainParameter;
  Priv: TCnDSAPrivateKey;
  Pub: TCnDSAPublicKey;
begin
  Param := TCnDSADomainParameter.Create;
  // 3072-256
  Param.P.SetHex('AF3BC456683B8C3F647C0C5D92883C77C8EE17C080C794C52F8DD06F1727315EE55836DEDFF825DC6771E272107F1B0329A4C648945D4DBF1BF9084B4'
    + '50CC0F07DB48AF8DED117E41D8D0C6EDE48569A1144DE8DA814C2DDC1B02C3D90DD0CD62A4A8672D8A76B92315FEFB5C6581991A4B012E73B862F20A28F2B25EDC13'
    + '550396B323B1D8EF471A4CE17EBE4E2850E681AE4EC64F314EAF0746EEB7870B3764E4BB7564DE5B3D1F1AB6570D4C141EC127350245016AFD24A3056088D543E39D'
    + '3C6C66879FAA7865C7E68EEAB2E061DF67559FCBC08C43D0FC7B030F52B277BAB127284B8CC6803EA7876B24A27E946CDBBD88B7207F01FCC366289AFBA356E2A45C'
    + '952562EFB9E21F2603D038FB2A591B2F86E986924FCAC5041722B0EA3609DDAEEE8D6512276567E79855DC34E091C7000D1E06BCD5FFEEE8371DECDD4FB495B23DD9'
    + '22B6AD087A78B97726A1E94D6F0EDCBF081335525211BF07644B6FAD02520BA9D9CB9D7E0CCE2162E4F501E5E46FBBE9A8EAF3E3222A07BA68B2BAB');
  Param.Q.SetHex('F95D06B019219D8FEA787B3FF8F422F79272E84946E4353B35F4566D7FDA03F3');
  Param.G.SetHex('6C2F378A1239DE24C5AB44ACA76767B28BD8814FB66E69DFE0E199CD8A784B7BD459864E585F765021772D07CA81D7E663EA5B58834721E6A6327ED4F'
    + '75CFC68E01D0D2F948919D13F7D4DE147B09D2624767562F9710C270634D729BBB3B6DD6BBF8D8B4DAA18A7DEDFB3D3288A29861C6D0C6B0D1FAB3BF633AD5002F3C'
    + 'A52557537860BFC80BEA9ABEEB636570343DF23DE1F4D5277842AC19A164880C87D2B6EE3653EF9B5A180AA7B7B72585EFB03ACE7B116A42C9AC6501B2CD0D428139'
    + '287C6AD48B73DD93C135C261C776E2F00EECF391D38D04A9CA827DD30A8552653DE24D23C44841FEA08CFAE624C83D7B84DD6DE1FB6D46176614FDD949AFBE1082A9'
    + '6AA9CBCADC7112F671954558F7B89C1DDD70E5DC4555B5C9E091138EFEF6192C3E04019C1E8406E3036FA3204A41AC9347F83DA658EDE5C1DC7B5D521ECEA0D9B736'
    + 'C4A03FCCD620F6D126D775C72BD3E0424FE69BDBEFD7AD37259C37AC24EB60A1DDBA2F05D8BE4A78D5B039DC2908B9EEC630DC465D9BC31A084B045');

  Priv := TCnDSAPrivateKey.Create;
  Priv.SetHex('8DF5713F314C2CDB52002416D0C9F097CB2EB5393B8823B089BA993D3C7E69BE');
  Pub := TCnDSAPublicKey.Create;
  Pub.SetHex('0C51F97FB2E2C08D46D25920B02F0617590CFB6218920FEEE348A506FD158D8AB25A64DE56E2AB32CA47CFA81585D65B536CA0A78EC94F2CBB2C2046E97EC'
    + 'E2515016CE6C5367293B05B4E9DBF0B5B07A11E2EA60E238D602F02D61E7F60DA6601F115945E6C94C931B1DA52D95B035EED97BA675BF73320FEEC3E32118F2C19B'
    + '07CE536CD0A37DA5886F2F4EC8227D906815898950A78C646C1B639AB1ECDC9D04995F8A140344D770C64C17EBA67B4C2FBBE6DA9D07C7EC2C1DFAB3C8705DDC1A77'
    + '4AAAC3C9991E278E78CA3077344981117E4933F968988E070F900977D33D3CA8904C25E68B754B0C62709C6A8E14B9AFF0D14A2417CF46733A5DB0EE3F6C47231F81'
    + 'B19F7248A1778D601031153353C93C623DACCAAF8C04C8DE06817B7343717E442F579BB709005988BF01D02A998C3A1685F4F573B9E88BEA6E2A0E354B4C4DF68886'
    + '26B119DCDA09983E505093F518F8156132AA59B52815B9B9BC1E4DEC8B875F0F05242E73530EE64861787D19FD0FA6BD40D3E93E65333B0E602');

  Result := CnDSAVerifyParameter(Param);
  if not Result then Exit;

  Result := CnDSAVerifyKeys(Param, Priv, Pub);

  Pub.Free;
  Priv.Free;
  Param.Free;
end;

function TestDSA3: Boolean;
var
  Param: TCnDSADomainParameter;
  Priv: TCnDSAPrivateKey;
  Pub: TCnDSAPublicKey;
  Data: TBytes;
  Sig: TCnDSASignature;
begin
  Param := TCnDSADomainParameter.Create;
  //2048-256
  Param.P.SetHex('93467C3E62C6DABAC12E4457FC85A5290E03D601CCF1D9ECFB7F9234991118EC9BB6C8D0A03E3E33799D437C96214AC8452A57095DDC41AAFD141333F'
    + '953AE68059355394ED7089C530F822DA52B12524149260349FF155E975E020B486AC8B51730C8AD63233249AC0FA9BFED08CF5D5EC23B58500FD65A23B7DC3A8A7AC'
    + '0397E08529EB5FF6224ED6B6DE5811C6815EBC72EDA44151D34D581C3DCA04B1DED3D889B6E149679879C1458A00B6F44F7F888DBA2D08637D8E1BBDA738224FDF60'
    + '0440D426A5F5E8A28C63AEEA464987884F2DB225C2DB154192A638F3D1FBBCF4D0ADDBE1CFA1091EB38D793798D946FFA314F22D0BD0A61E1770BCD8DD65039');
  Param.Q.SetHex('C32158E5674E2F8B2BF1C49438EEE81BC3FF091E08A0F7036C680B8DBF95ED0F');
  Param.G.SetHex('3C78A579FF4458BFD24433AA0F3096183FF59C71A0B5AA1621B6E37B2C803E628CDA1AC792443EB27F33E0C282DF76C4C976AC716B9E61C5F32F6A822'
    + '518F2A79EEEEC19073DDDE46057A899BC0A0FA58DFD90CC160B67AC1FBAF808C28474A2F056E994A42012C29DA0ABFBFB2DEFF04B4982D00AC4620C6FBD9AD403D46'
    + '6C9017A87B53F5082F469417F99DE185603859BF837220ED5D4649965A79E77CD1B1E3E73A2C9B39E64C6A9D08703DACD5C9352074335EC22935DCCEFBF5E6917316'
    + 'E2B5E400AD7F06E48AF6CA9EECD83C7D3F59E2A4A5283D9E7B20FCD1B3D7B9C6963FE47B602E4FC36A1106F388262DAF195CE84DF5B59050CCE80EDADD9DD47');

  Priv := TCnDSAPrivateKey.Create;
  Priv.SetHex('A58A5D6357E4BA2ACD2ADC0DD58E687889AC9AF134A2DF74917BB2115D00DFE6');
  Pub := TCnDSAPublicKey.Create;
  Pub.SetHex('1F944347D5D0931B387957310CC6EF189685214163A3C8D7AC7789C2C401843CCCED8BBDAD4E4108DA7919A9017B20B26AADD25A2F3F3E69E1BEFD42F6DA0'
    + 'E0C1CE940802DE1BFEB164373EB6982E60FF0D64C9DCDFE23430ECC646E54B97F52204DE31E50587C62F3FBA966EA3D2A26BF6B785CA1FD5067D2913FD50A40BF412'
    + '8EE2E6C5806D22E5FDA64D5CEE38D8C66C9E572F0122490282E48463686919AE404D8B0EF54DFAADDE857BC3EA4CAEACAF4C36AE24C238F81174FB8ED9870D3B2353'
    + 'F62FE485E33EC411F7936BA9335BD31F7E0471D4EA84EA9B362016871983EE2788AB2BF2ED633D9343CD1042460C216A7D238BDB5E2916F71794FC548A7');

  Result := CnDSAVerifyParameter(Param);
  if not Result then Exit;

  Result := CnDSAVerifyKeys(Param, Priv, Pub);
  if not Result then Exit;

  Data := AnsiToBytes('CnPack Test Data for DSA');
  Sig := TCnDSASignature.Create;

  Result := CnDSASignBytes(Data, Param, Priv, Sig);
  if not Result then Exit;

  Result := CnDSAVerifyBytes(Data, Param, Pub, Sig);

  Sig.Free;
  Pub.Free;
  Priv.Free;
  Param.Free;
end;

// ================================ PDF ========================================

function TestPDFCalcOwnerPassword: Boolean;
var
  O: TBytes;
begin
  O := CnPDFCalcOwnerCipher('123456', '654321', 4, 4, 128);
  Result := BytesToHex(O) = 'C336FDBECB59F7B59C244B61B745F71AC5BA427B1B9102DA468E77127F1E69D6';
end;

function TestPDFCalcUserPassword: Boolean;
var
  U, O: TBytes;
begin
  O := HexToBytes('C336FDBECB59F7B59C244B61B745F71AC5BA427B1B9102DA468E77127F1E69D6');
  U := CnPDFCalcUserCipher('654321', 4, 4, O, Cardinal(-3904), HexToBytes('04EDE6407FAD4026986F3452ECA1AC62'), 128);

  Result := CompareBytes(U, HexToBytes('873B7BBDD6A0A4BCE10C44E26BD20E4F'), 16);
end;

function TestPDFCheckOwnerPassword: Boolean;
var
  OC, UC, ID, Key: TBytes;
begin
  OC := HexToBytes('B6DC51AF84CDB5A22DD5FC390618A0F8E16CAB8AF14E67CCBA5F90837AAC898B');
  UC := HexToBytes('66AE712E6DF1888690C8CCAFF51B460BAFEE54CC25933740AFCBC7E71EA4ED99');
  ID := HexToBytes('446C6A93022D972DEC265D7B398D54A7');

  Key := CnPDFCheckOwnerPassword('123456', 1, 2, OC, UC, Cardinal(-64), ID, 0);
  Result := CompareBytes(Key, HexToBytes('FDE36836FF'), 16);
end;

// ================================ SM2 ========================================

function TestSM21: Boolean;
var
  M, U: AnsiString;
  Priv: TCnSM2PrivateKey;
  Pub: TCnSM2PublicKey;
  Sig: TCnSM2Signature;
begin
  // SM2 签名验签，《SM2椭 圆曲线公钥密码算法第五部分参数定义》中的例子
  M := 'message digest';
  U := '1234567812345678';

  Priv := nil;
  Pub := nil;
  Sig := nil;

  try
    Priv := TCnSM2PrivateKey.Create;
    Pub := TCnSM2PublicKey.Create;
    Sig := TCnSM2Signature.Create;

    Priv.SetHex('3945208F7B2144B13F36E38AC6D39F95889393692860B51A42FB81EF4DF7C5B8');
    Pub.X.SetHex('09F9DF311E5421A150DD7D161E4BC5C672179FAD1833FC076BB08FF356F35020');
    Pub.Y.SetHex('CCEA490CE26775A52DC6EA718CC1AA600AED05FBF35E084A6632F6072DA9AD13');

    Result := CnSM2CheckKeys(Priv, Pub);
    if not Result then Exit;

    if CnSM2SignData(U, @M[1], Length(M), Sig, Priv, Pub, nil, '59276E27D506861A16680F3AD9C02DCCEF3CC1FA3CDBE4CE6D54B80DEAC1BC21') then
    begin
      Result := Sig.ToHex() = 'F5A03B0648D2C4630EEAC513E1BB81A15944DA3827D5B74143AC7EACEEE720B3' +
        'B1B6AA29DF212FD8763182BC0D421CA1BB9038FD1F7F42D4840B69C485BBC1AA';

      if not Result then Exit;

      Result := CnSM2VerifyData(U, @M[1], Length(M), Sig, Pub);
    end;
  finally
    Sig.Free;
    Pub.Free;
    Priv.Free;
  end;
end;

function TestSM22: Boolean;
var
  M: AnsiString;
  Priv: TCnSM2PrivateKey;
  Pub: TCnSM2PublicKey;
  EnStream, DeStream: TMemoryStream;
begin
  // SM2 加密解密，《SM2椭 圆曲线公钥密码算法第五部分参数定义》中的例子
  M := 'encryption standard';

  Priv := nil;
  Pub := nil;
  EnStream := nil;
  DeStream := nil;

  try
    Priv := TCnSM2PrivateKey.Create;
    Pub := TCnSM2PublicKey.Create;
    EnStream := TMemoryStream.Create;

    Priv.SetHex('3945208F7B2144B13F36E38AC6D39F95889393692860B51A42FB81EF4DF7C5B8');
    Pub.X.SetHex('09F9DF311E5421A150DD7D161E4BC5C672179FAD1833FC076BB08FF356F35020');
    Pub.Y.SetHex('CCEA490CE26775A52DC6EA718CC1AA600AED05FBF35E084A6632F6072DA9AD13');

    Result := False;
    if CnSM2EncryptData(@M[1], Length(M), EnStream, Pub, nil, cstC1C3C2, True, '59276E27D506861A16680F3AD9C02DCCEF3CC1FA3CDBE4CE6D54B80DEAC1BC21') then
    begin
      Result := DataToHex(EnStream.Memory, EnStream.Size) = '04' +
        '04EBFC718E8D1798620432268E77FEB6415E2EDE0E073C0F4F640ECD2E149A73' +
        'E858F9D81E5430A57B36DAAB8F950A3C64E6EE6A63094D99283AFF767E124DF0' +
        '59983C18F809E262923C53AEC295D30383B54E39D609D160AFCB1908D0BD8766' +
        '21886CA989CA9C7D58087307CA93092D651EFA';

      if not Result then Exit;

      DeStream := TMemoryStream.Create;
      if CnSM2DecryptData(EnStream.Memory, EnStream.Size, DeStream, Priv) then
        Result := CompareMem(DeStream.Memory, @M[1], DeStream.Size);
    end;
  finally
    DeStream.Free;
    EnStream.Free;
    Pub.Free;
    Priv.Free;
  end;
end;

function TestSM23: Boolean;
const
  KEY_LENGTH = 16;
  AID = '12341234';
  BID = '43214321';
var
  APrivateKey, BPrivateKey: TCnSM2PrivateKey;
  APublicKey, BPublicKey: TCnSM2PublicKey;
  RandA, RandB: TCnBigNumber;
  OutRA, OutRB: TCnEccPoint;
  KA, KB: TBytes;
  OpSA, OpSB, OpS2: TCnSM3Digest;
begin
  APrivateKey := TCnSM2PrivateKey.Create;
  APublicKey := TCnSM2PublicKey.Create;
  BPrivateKey := TCnSM2PrivateKey.Create;
  BPublicKey := TCnSM2PublicKey.Create;

  RandA := TCnBigNumber.Create;
  RandB := TCnBigNumber.Create;
  OutRA := TCnEccPoint.Create;
  OutRB := TCnEccPoint.Create;

  try
    APrivateKey.SetHex('81EB26E941BB5AF16DF116495F90695272AE2CD63D6C4AE1678418BE48230029');
    APublicKey.X.SetHex('160E12897DF4EDB61DD812FEB96748FBD3CCF4FFE26AA6F6DB9540AF49C94232');
    APublicKey.Y.SetHex('4A7DAD08BB9A459531694BEB20AA489D6649975E1BFCF8C4741B78B4B223007F');
    BPrivateKey.SetHex('785129917D45A9EA5437A59356B82338EAADDA6CEB199088F14AE10DEFA229B5');
    BPublicKey.X.SetHex('6AE848C57C53C7B1B5FA99EB2286AF078BA64C64591B8B566F7357D576F16DFB');
    BPublicKey.Y.SetHex('EE489D771621A27B36C5C7992062E9CD09A9264386F3FBEA54DFF69305621C4D');

    // Step1
    Result := CnSM2KeyExchangeAStep1(AID, BID, KEY_LENGTH,
      APrivateKey, APublicKey, BPublicKey, RandA, OutRA);
    if not Result then Exit;

    // Step2
    Result := CnSM2KeyExchangeBStep1(AID, BID, KEY_LENGTH,
      BPrivateKey, APublicKey, BPublicKey, OutRA, KB, OutRB, OpSB, OpS2);
    if not Result then Exit;

    // Step3
    Result := CnSM2KeyExchangeAStep2(AID, BID, KEY_LENGTH,
      APrivateKey, APublicKey, BPublicKey, OutRA, OutRB, RandA, KA, OpSB, OpSA);
    if not Result then Exit;

    // Step4
    Result := CnSM2KeyExchangeBStep2(AID, BID, KEY_LENGTH,
      BPrivateKey, APublicKey, BPublicKey, OpSA, OpS2);
    if not Result then Exit;

    Result := CompareBytes(KA, KB);
  finally
    OutRA.Free;
    OutRB.Free;
    RandA.Free;
    RandB.Free;

    APublicKey.Free;
    APrivateKey.Free;
    BPublicKey.Free;
    BPrivateKey.Free;
  end;
end;

// ================================ SM3 ========================================

function TestSM3: Boolean;
var
  Dig: TCnSM3Digest;
  Data: TBytes;
begin
  Data := HexToBytes('436E5061636B2054657374');
  Dig := SM3Bytes(Data);
  Result := DataToHex(@Dig[0], SizeOf(TCnSM3Digest)) = '3E956CABBF6D874D00F9CCF2C993C7BDDBC5AEF373C2D5E7BFA99B847289653F';
end;

function TestSM3HMac: Boolean;
var
  S: AnsiString;
  Dig: TCnSM3Digest;
  Data: TBytes;
begin
  S := 'CnPack Key';
  Data := HexToBytes('436E5061636B2054657374');
  SM3Hmac(@S[1], Length(S), @Data[0], Length(Data), Dig);
  Result := DataToHex(@Dig[0], SizeOf(TCnSM3Digest)) = '393FFDFADE8A0E6ADFF832E6E126B2713EEB48066FEA8963CF63C258F65E368F';
end;

function TestSM3Update: Boolean;
var
  D1, D2: TCnSM3Digest;
  C: TCnSM3Context;
  S, S1, S2: AnsiString;
begin
  S1 := '0123456789abcdefghi';
  S2 := 'jklmnop';
  S := S1 + S2;

  D1 := SM3StringA(S);
  SM3Init(C);
  SM3Update(C, PAnsiChar(S1), Length(S1));
  SM3Update(C, PAnsiChar(S2), Length(S2));
  SM3Final(C, D2);

  Result := SM3Match(D1, D2);
end;

// ================================ SM9 ========================================

function TestSM9Hash1: Boolean;
var
  SM9: TCnSM9;
  S: AnsiString;
  Res: TCnBigNumber;
begin
  // SM9 相关杂凑之一，《SM9 标识密码算法第 5 部分：参数定义》中的签名部分与密钥交换部分的杂凑例子
  SM9 := TCnSM9.Create;
  S := 'Alice' + #1;
  Res := TCnBigNumber.Create;

  CnSM9Hash1(Res, @S[1], Length(S), SM9.Order);
  Result := Res.ToHex() = '2ACC468C3926B0BDB2767E99FF26E084DE9CED8DBC7D5FBF418027B667862FAB';
  if not Result then Exit;

  S := 'Alice' + #2;
  CnSM9Hash1(Res, @S[1], Length(S), SM9.Order);
  Result := Res.ToHex() = 'A9AC0FDA7380ED8E3325FDDCD40A7221E3CD72F6FFA7F27D54AD494CEDB4E212';

  Res.Free;
  SM9.Free;
end;

function TestSM9Hash2: Boolean;
var
  SM9: TCnSM9;
  Data: TBytes;
  Res: TCnBigNumber;
begin
  // SM9 相关杂凑之二，《SM9 标识密码算法第 5 部分：参数定义》中的密钥交换部分的杂凑例子
  SM9 := TCnSM9.Create;
  Data := HexToBytes('4368696E65736520494253207374616E6461726481377B8FDBC2839B4FA2D0E0F8AA6853BBBE9E9C' +
    '4099608F8612C6078ACD7563815AEBA217AD502DA0F48704CC73CABB3C06209BD87142E14CBD99E8' +
    'BCA1680F30DADC5CD9E207AEE32209F6C3CA3EC0D800A1A42D33C73153DED47C70A39D2E8EAF5D17' +
    '9A1836B359A9D1D9BFC19F2EFCDB829328620962BD3FDF15F2567F58A543D25609AE943920679194' +
    'ED30328BB33FD15660BDE485C6B79A7B32B013983F012DB04BA59FE88DB889321CC2373D4C0C35E8' +
    '4F7AB1FF33679BCA575D67654F8624EB435B838CCA77B2D0347E65D5E46964412A096F4150D8C5ED' +
    'E5440DDF0656FCB663D24731E80292188A2471B8B68AA993899268499D23C89755A1A89744643CEA' +
    'D40F0965F28E1CD2895C3D118E4F65C9A0E3E741B6DD52C0EE2D25F5898D60848026B7EFB8FCC1B2' +
    '442ECF0795F8A81CEE99A6248F294C82C90D26BD6A814AAF475F128AEF43A128E37F80154AE6CB92' +
    'CAD7D1501BAE30F750B3A9BD1F96B08E97997363911314705BFB9A9DBB97F75553EC90FBB2DDAE53' +
    'C8F68E42');
  Res := TCnBigNumber.Create;

  CnSM9Hash2(Res, @Data[0], Length(Data), SM9.Order);
  Result := Res.ToHex() = '823C4B21E4BD2DFE1ED92C606653E996668563152FC33F55D7BFBB9BD9705ADB';

  Res.Free;
  SM9.Free;
end;

function TestSM9Mac: Boolean;
var
  K, C: TBytes;
  D: TCnSM3Digest;
begin
  // SM9 相关杂凑 MAC，《SM9 标识密码算法第 5 部分：参数定义》中的加密部分的杂凑例子
  K := HexToBytes('8651FFD5F738003E51DF31174D0E4E402FD87F4581B612F74259DB574F67ECE6');
  C := HexToBytes('1B5F5B0E951489682F3E64E1378CDD5DA9513B1C');
  D := SM9Mac(@K[0], Length(K), @C[0], Length(C));
  Result := DataToHex(@D[0], SizeOf(TCnSM3Digest)) = 'BA672387BCD6DE5016A158A52BB2E7FC429197BCAB70B25AFEE37A2B9DB9F367';
end;

function TestSM9Sign: Boolean;
var
  SigMasterKey: TCnSM9SignatureMasterKey;
  SigUserKey: TCnSM9SignatureUserPrivateKey;
  Sig: TCnSM9Signature;
  AP: TCnFP2AffinePoint;
  SM9: TCnSM9;
  User, S: AnsiString;
begin
  // 《SM9 标识密码算法第 5 部分：参数定义》中的附录 A 中的签名验签例子
  SM9 := TCnSM9.Create;
  SigMasterKey := TCnSM9SignatureMasterKey.Create;
  SigUserKey := TCnSM9SignatureUserPrivateKey.Create;
  Sig := TCnSM9Signature.Create;
  AP := TCnFP2AffinePoint.Create;

  try
    // 示例 Master Key
    SigMasterKey.PrivateKey.SetHex('0130E78459D78545CB54C587E02CF480CE0B66340F319F348A1D5B1F2DC5F4');
    FP2PointToFP2AffinePoint(AP, SM9.Generator2);

    FP2AffinePointMul(AP, AP, SigMasterKey.PrivateKey, SM9.FiniteFieldSize);
    FP2AffinePointToFP2Point(SigMasterKey.PublicKey, AP, SM9.FiniteFieldSize);

    // 核对 Master Key
    Result := (SigMasterKey.PublicKey.X.ToString = '9F64080B3084F733E48AFF4B41B565011CE0711C5E392CFB0AB1B6791B94C408,29DBA116152D1F786CE843ED24A3B573414D2177386A92DD8F14D65696EA5E32')
      and (SigMasterKey.PublicKey.Y.ToString = '69850938ABEA0112B57329F447E3A0CBAD3E2FDB1A77F335E89E1408D0EF1C25,41E00A53DDA532DA1A7CE027B7A46F741006E85F5CDFF0730E75C05FB4E3216D');
    if not Result then Exit;

    // 生成示例 User Key
    User := 'Alice';
    CnSM9KGCGenerateSignatureUserKey(SigMasterKey.PrivateKey, User, SigUserKey);

    // 核对 User Key
    Result := SigUserKey.ToHex = '04A5702F05CF1315305E2D6EB64B0DEB923DB1A0BCF0CAFF90523AC8754AA6982078559A844411F9825C109F5EE3F52D720DD01785392A727BB1556952B2B013D3';
    if not Result then Exit;

    S := 'Chinese IBS standard';

    // 签名
    Result := CnSM9UserSignData(SigMasterKey.PublicKey, SigUserKey, @S[1], Length(S), Sig,
      nil, '033C8616B06704813203DFD00965022ED15975C662337AED648835DC4B1CBE');
    if not Result then Exit;

    Result := (Sig.H.ToHex = '823C4B21E4BD2DFE1ED92C606653E996668563152FC33F55D7BFBB9BD9705ADB')
      and (Sig.S.ToHex = '0473BF96923CE58B6AD0E13E9643A406D8EB98417C50EF1B29CEF9ADB48B6D598C856712F1C2E0968AB7769F42A99586AED139D5B8B3E15891827CC2ACED9BAA05');
    if not Result then Exit;

    // 验证
    Result := CnSM9UserVerifyData(User, @S[1], Length(S), Sig, SigMasterKey.PublicKey);
  finally
    AP.Free;
    Sig.Free;
    SigUserKey.Free;
    SigMasterKey.Free;
    SM9.Free;
  end;
end;

function TestSM9KeyExchange: Boolean;
const
  KLEN = 16;
var
  AUser, BUser: AnsiString;
  RA, RB: TCnEccPoint;
  RandA, RandB: TCnBigNumber;
  BG1, BG2, BG3: TCnFP12;
  KeyA, KeyB: TBytes;
  SB, SA: TCnSM3Digest;
  KeyExchangeMasterKey: TCnSM9KeyExchangeMasterKey;
  KeyExchangeUserA, KeyExchangeUserB: TCnSM9KeyExchangeUserPrivateKey;
begin
  // 《SM9 标识密码算法第 5 部分：参数定义》中的附录 B 中的密钥交换例子
  KeyExchangeMasterKey := TCnSM9KeyExchangeMasterKey.Create;
  KeyExchangeUserA := TCnSM9KeyExchangeUserPrivateKey.Create;
  KeyExchangeUserB := TCnSM9KeyExchangeUserPrivateKey.Create;

  // 设置示例 Master Key
  KeyExchangeMasterKey.PrivateKey.SetHex('02E65B0762D042F51F0D23542B13ED8CFA2E9A0E7206361E013A283905E31F');
  KeyExchangeMasterKey.PublicKey.X.SetHex('9174542668E8F14AB273C0945C3690C66E5DD09678B86F734C4350567ED06283');
  KeyExchangeMasterKey.PublicKey.Y.SetHex('54E598C6BF749A3DACC9FFFEDD9DB6866C50457CFC7AA2A4AD65C3168FF74210');

  // 生成示例 User Key
  AUser := 'Alice';
  BUser := 'Bob';
  CnSM9KGCGenerateKeyExchangeUserKey(KeyExchangeMasterKey.PrivateKey, AUser, KeyExchangeUserA);
  CnSM9KGCGenerateKeyExchangeUserKey(KeyExchangeMasterKey.PrivateKey, BUser, KeyExchangeUserB);

  Result := (KeyExchangeUserA.X.ToString = '0FE8EAB395199B56BF1D75BD2CD610B6424F08D1092922C5882B52DCD6CA832A,7DA57BC50241F9E5BFDDC075DD9D32C7777100D736916CFC165D8D36E0634CD7')
    and (KeyExchangeUserA.Y.ToString = '83A457DAF52CAD464C903B26062CAF937BB40E37DADED9EDA401050E49C8AD0C,6970876B9AAD1B7A50BB4863A11E574AF1FE3C5975161D73DE4C3AF621FB1EFB')
    and (KeyExchangeUserB.X.ToString = '74CCC3AC9C383C60AF083972B96D05C75F12C8907D128A17ADAFBAB8C5A4ACF7,01092FF4DE89362670C21711B6DBE52DCD5F8E40C6654B3DECE573C2AB3D29B2')
    and (KeyExchangeUserB.Y.ToString = '44B0294AA04290E1524FF3E3DA8CFD432BB64DE3A8040B5B88D1B5FC86A4EBC1,8CFC48FB4FF37F1E27727464F3C34E2153861AD08E972D1625FC1A7BD18D5539');
  if not Result then Exit;

  // 开始交换
  RA := nil;
  RandA := nil;
  RB := nil;
  RandB := nil;

  BG1 := nil;
  BG2 := nil;
  BG3 := nil;

  try
    // 第一步，A 调用
    RA := TCnEccPoint.Create;
    RandA := TCnBigNumber.Create;

    Result := CnSM9UserKeyExchangeAStep1(BUser, KLEN, KeyExchangeMasterKey.PublicKey, RA, RandA, nil,
      '5879DD1D51E175946F23B1B41E93BA31C584AE59A426EC1046A4D03B06C8');
    if not Result then Exit;

    Result := (RandA.ToHex = '5879DD1D51E175946F23B1B41E93BA31C584AE59A426EC1046A4D03B06C8')
      and (RA.X.ToHex = '7CBA5B19069EE66AA79D490413D11846B9BA76DD22567F809CF23B6D964BB265')
      and (RA.Y.ToHex = 'A9760C99CB6F706343FED05637085864958D6C90902ABA7D405FBEDF7B781599');
    if not Result then Exit;

    // 第二步，B 调用，使用了第一步里传来的 RA
    RB := TCnEccPoint.Create;
    BG1 := TCnFP12.Create;
    BG2 := TCnFP12.Create;
    BG3 := TCnFP12.Create;
    Result := CnSM9UserKeyExchangeBStep1(AUser, BUser, KLEN, KeyExchangeMasterKey.PublicKey,
      KeyExchangeUserB, RA, RB, KeyB, SB, BG1, BG2, BG3, nil, '018B98C44BEF9F8537FB7D071B2C928B3BC65BD3D69E1EEE213564905634FE');
    if not Result then Exit;

    Result := (RB.X.ToHex = '861E91485FB7623D2794F495031A35598B493BD45BE37813ABC710FCC1F34482')
      and (RB.Y.ToHex = '32D906A469EBC1216A802A7052D5617CD430FB56FBA729D41D9BD668E9EB9600')
      and (DataToHex(@SB[0], SizeOf(TCnSM3Digest)) = '3BB4BCEE8139C960B4D6566DB1E0D5F0B2767680E5E1BF934103E6C66E40FFEE')
      and (DataToHex(@KeyB[0], Length(KeyB)) = 'C5C13A8F59A97CDEAE64F16A2272A9E7');
    if not Result then Exit;

    // BG1、BG2、BG3 的判断太长，省略

    // 第三步，A 调用，使用了第二步里传过来的 RB 和 SB 以及第一步自身的 RandA
    Result := CnSM9UserKeyExchangeAStep2(AUser, BUser, KLEN, KeyExchangeMasterKey.PublicKey,
      KeyExchangeUserA, RandA, RA, RB, SB, KeyA, SA);
    if not Result then Exit;

    Result := (DataToHex(@KeyA[0], Length(KeyA)) = 'C5C13A8F59A97CDEAE64F16A2272A9E7')
      and (DataToHex(@SA[0], SizeOf(TCnSM3Digest)) = '195D1B7256BA7E0E67C71202A25F8C94FF8241702C2F55D613AE1C6B98215172');
    if not Result then Exit;

    // 第四步，B 调用，使用了第一步里传过来的 RA 以及第二步自身的 BG1、BG2、BG3、RB 以及第三步里传过来的 SA
    Result := CnSM9UserKeyExchangeBStep2(AUser, BUser, RA, RB, SA, BG1, BG2, BG3);
    if not Result then Exit;

    Result := CompareBytes(KeyA, KeyB);
  finally
    BG3.Free;
    BG2.Free;
    BG1.Free;
    RandB.Free;
    RB.Free;
    RandA.Free;
    RA.Free;

    KeyExchangeUserB.Free;
    KeyExchangeUserA.Free;
    KeyExchangeMasterKey.Free;
  end;
end;

function TestSM9KeyEncapsulation: Boolean;
var
  MasterKey: TCnSM9EncryptionMasterKey;
  UserKey: TCnSM9EncryptionUserPrivateKey;
  KeyEncapsulation: TCnSM9KeyEncapsulation;
  Key: TBytes;
begin
  // 《SM9 标识密码算法第 5 部分：参数定义》中的附录 C 中的密钥封装例子
  MasterKey := TCnSM9EncryptionMasterKey.Create;
  UserKey := TCnSM9EncryptionUserPrivateKey.Create;
  KeyEncapsulation := TCnSM9KeyEncapsulation.Create;

  try
    MasterKey.PrivateKey.SetHex('01EDEE3778F441F8DEA3D9FA0ACC4E07EE36C93F9A08618AF4AD85CEDE1C22');
    MasterKey.PublicKey.X.SetHex('787ED7B8A51F3AB84E0A66003F32DA5C720B17ECA7137D39ABC66E3C80A892FF');
    MasterKey.PublicKey.Y.SetHex('769DE61791E5ADC4B9FF85A31354900B202871279A8C49DC3F220F644C57A7B1');

    Result := CnSM9KGCGenerateEncryptionUserKey(MasterKey.PrivateKey, 'Bob', UserKey);
    if not Result then Exit;

    Result := (UserKey.X.ToString = '94736ACD2C8C8796CC4785E938301A139A059D3537B6414140B2D31EECF41683,115BAE85F5D8BC6C3DBD9E5342979ACCCF3C2F4F28420B1CB4F8C0B59A19B158')
      and (UserKey.Y.ToString = '7AA5E47570DA7600CD760A0CF7BEAF71C447F3844753FE74FA7BA92CA7D3B55F,27538A62E7F7BFB51DCE08704796D94C9D56734F119EA44732B50E31CDEB75C1');
    if not Result then Exit;

    Result := CnSM9UserSendKeyEncapsulation('Bob', 32, MasterKey.PublicKey, KeyEncapsulation, nil, '74015F8489C01EF4270456F9E6475BFB602BDE7F33FD482AB4E3684A6722');
    if not Result then Exit;

    Result := (BytesToHex(KeyEncapsulation.Key) = '4FF5CF86D2AD40C8F4BAC98D76ABDBDE0C0E2F0A829D3F911EF5B2BCE0695480')
      and (KeyEncapsulation.Code.X.ToHex = '1EDEE2C3F465914491DE44CEFB2CB434AB02C308D9DC5E2067B4FED5AAAC8A0F')
      and (KeyEncapsulation.Code.Y.ToHex = '1C9B4C435ECA35AB83BB734174C0F78FDE81A53374AFF3B3602BBC5E37BE9A4C');
    if not Result then Exit;

    Result := CnSM9UserReceiveKeyEncapsulation('Bob', UserKey, 32, KeyEncapsulation.Code, Key);
    if not Result then Exit;

    Result := BytesToHex(Key) = '4FF5CF86D2AD40C8F4BAC98D76ABDBDE0C0E2F0A829D3F911EF5B2BCE0695480';
  finally
    KeyEncapsulation.Free;
    UserKey.Free;
    MasterKey.Free;
  end;
end;

function TestSM9PublicEncryption: Boolean;
var
  User, S: AnsiString;
  EnStream, DeStream: TMemoryStream;
  KeyEncMasterKey: TCnSM9EncryptionMasterKey;
  KeyEncUserKey: TCnSM9EncryptionUserPrivateKey;
begin
  // 《SM9 标识密码算法第 5 部分：参数定义》中的附录 D 中的公钥加密私钥解密的例子
  KeyEncMasterKey := TCnSM9EncryptionMasterKey.Create;
  KeyEncUserKey := TCnSM9EncryptionUserPrivateKey.Create;

  EnStream := TMemoryStream.Create;
  DeStream := TMemoryStream.Create;

  try
    // 生成示例 Master Key
    KeyEncMasterKey.PrivateKey.SetHex('01EDEE3778F441F8DEA3D9FA0ACC4E07EE36C93F9A08618AF4AD85CEDE1C22');
    KeyEncMasterKey.PublicKey.X.SetHex('787ED7B8A51F3AB84E0A66003F32DA5C720B17ECA7137D39ABC66E3C80A892FF');
    KeyEncMasterKey.PublicKey.Y.SetHex('769DE61791E5ADC4B9FF85A31354900B202871279A8C49DC3F220F644C57A7B1');

    // 生成示例 User Key
    User := 'Bob';
    CnSM9KGCGenerateEncryptionUserKey(KeyEncMasterKey.PrivateKey, User, KeyEncUserKey);

    Result := (KeyEncUserKey.X.ToString = '94736ACD2C8C8796CC4785E938301A139A059D3537B6414140B2D31EECF41683,115BAE85F5D8BC6C3DBD9E5342979ACCCF3C2F4F28420B1CB4F8C0B59A19B158')
      and (KeyEncUserKey.Y.ToString = '7AA5E47570DA7600CD760A0CF7BEAF71C447F3844753FE74FA7BA92CA7D3B55F,27538A62E7F7BFB51DCE08704796D94C9D56734F119EA44732B50E31CDEB75C1');
    if not Result then Exit;

    S := 'Chinese IBE standard';

    Result := CnSM9UserEncryptData(User, KeyEncMasterKey.PublicKey, @S[1], Length(S), 16, 32, EnStream, semSM4, nil,
      'AAC0541779C8FC45E3E2CB25C12B5D2576B2129AE8BB5EE2CBE5EC9E785C');
    if not Result then Exit;

    Result := StreamToHex(EnStream) = '2445471164490618E1EE20528FF1D545B0F14C8BCAA44544F03DAB5DAC07D8FF42FFCA97'
      + 'D57CDDC05EA405F2E586FEB3A6930715532B8000759F13059ED59AC0FD3C98DD92C44C68'
      + '332675A370CCEEDE31E0C5CD209C257601149D12B394A2BEE05B6FAC6F11B965268C994F'
      + '00DBA7A8BB00FD60583546CBDF4649250863F10A';
    if not Result then Exit;

    Result := CnSM9UserDecryptData(User, KeyEncUserKey, EnStream.Memory, EnStream.Size, 32, DeStream, semSM4);
    if not Result then Exit;

    Result := StreamToHex(DeStream) = '4368696E65736520494245207374616E64617264';
    if not Result then Exit;

    EnStream.Clear;
    DeStream.Clear;
    Result := CnSM9UserEncryptData(User, KeyEncMasterKey.PublicKey, @S[1], Length(S), 16, 32, EnStream, semKDF, nil,
      'AAC0541779C8FC45E3E2CB25C12B5D2576B2129AE8BB5EE2CBE5EC9E785C');
    if not Result then Exit;

    Result := StreamToHex(EnStream) = '2445471164490618E1EE20528FF1D545B0F14C8BCAA44544F03DAB5DAC07D8FF42FFCA97'
      + 'D57CDDC05EA405F2E586FEB3A6930715532B8000759F13059ED59AC0BA672387BCD6DE50'
      + '16A158A52BB2E7FC429197BCAB70B25AFEE37A2B9DB9F3671B5F5B0E951489682F3E64E1'
      + '378CDD5DA9513B1C';
    if not Result then Exit;

    Result := CnSM9UserDecryptData(User, KeyEncUserKey, EnStream.Memory, EnStream.Size, 32, DeStream, semKDF);
    if not Result then Exit;

    Result := StreamToHex(DeStream) = '4368696E65736520494245207374616E64617264';
  finally
    EnStream.Free;
    DeStream.Free;
    KeyEncUserKey.Free;
    KeyEncMasterKey.Free;
  end;
end;

// ================================ RSA ========================================

function TestRSA1: Boolean;
var
  Priv: TCnRSAPrivateKey;
  Pub: TCnRSAPublicKey;
  Data, Sign: TMemoryStream;
  S: AnsiString;
begin
  Priv := TCnRSAPrivateKey.Create;
  Pub := TCnRSAPublicKey.Create;

  Priv.PrimeKey1.SetDec('11926501752010836305573547055010388132818881584424893368438260986340083815661726132764041431839102912426991427194974395769462431410735199955905343985824903');
  Priv.PrimeKey2.SetDec('8573695372469847739125271816213638824277647604717881633559571969225574325225015620887808957893360658142977333484565099818973457402882454768789495637432799');
  Priv.PrivKeyProduct.SetDec('102254192880968838810801382868389181256002534349148386664790879432128469476008930592645952145670063316387886310707201792269823813875471904969765005471'
    + '655784895966560585402535313253239266974336591272564426699631037042766443337890644687331420209218848611174173514131044514861915637897961332996331736169243193497');
  Priv.PrivKeyExponent.SetDec('39672511136982081334852781820872662950644314523029678314931072391484666575621085465907939411446247157266807837135542059783707067998407374882375067429'
    + '509912283003943601853564564341100084349650097087748956228566192837448482334604835364484274776995406202376013966890951221884425618589294581045684761888524743389');

  Pub.PubKeyExponent.SetDec('65537');
  Pub.PubKeyProduct.SetDec('10225419288096883881080138286838918125600253434914838666479087943212846947600893059264595214567006331638788631070720179226982381387547190496976500547165'
    + '5784895966560585402535313253239266974336591272564426699631037042766443337890644687331420209218848611174173514131044514861915637897961332996331736169243193497');

  Data := TMemoryStream.Create;
  Sign := TMemoryStream.Create;

  S := 'Data To Sign.';
  Data.Write(S[1], Length(S));
  Data.Position := 0;
  Result := CnRSASignStream(Data, Sign, Priv, rsdtSHA256);
  if not Result then Exit;

  Data.Position := 0;
  Sign.Position := 0;
  Result := CnRSAVerifyStream(Data, Sign, Pub, rsdtSHA256);

  Sign.Free;
  Data.Free;
  Pub.Free;
  Priv.Free;
end;

function TestRSA2: Boolean;
var
  Prime, Root, APriv, BPriv, APub, BPub, ASec, BSec: TCnBigNumber;
begin
  Prime := nil;
  Root := nil;
  APriv := nil;
  BPriv := nil;
  APub := nil;
  BPub := nil;
  ASec := nil;
  BSec := nil;

  try
    Prime := BigNumberFromDec('15068667503162894099');
    Root := BigNumberFromDec('2');

    APriv := BigNumberFromDec('123456');
    BPriv := BigNumberFromDec('654321');

    APub := BigNumberNew;
    BPub := BigNumberNew;

    Result := CnDiffieHellmanGenerateOutKey(Prime, Root, APriv, APub);
    if not Result then Exit;

    Result := CnDiffieHellmanGenerateOutKey(Prime, Root, BPriv, BPub);
    if not Result then Exit;

    ASec := BigNumberNew;
    BSec := BigNumberNew;

    Result := CnDiffieHellmanComputeKey(Prime, APriv, BPub, ASec);
    if not Result then Exit;

    Result := CnDiffieHellmanComputeKey(Prime, BPriv, APub, BSec);
    if not Result then Exit;

    Result := BigNumberEqual(ASec, BSec);
  finally
    Prime.Free;
    Root.Free;
    APriv.Free;
    BPriv.Free;
    APub.Free;
    BPub.Free;
    ASec.Free;
    BSec.Free;
  end;
end;

function TestRSA3: Boolean;
var
  Priv: TCnRSAPrivateKey;
  Pub: TCnRSAPublicKey;
  Data, E, D: TCnBigNumber;
begin
  Priv := TCnRSAPrivateKey.Create(True); // 启用 CRT
  Pub := TCnRSAPublicKey.Create;

  Priv.PrimeKey1.SetDec('11926501752010836305573547055010388132818881584424893368438260986340083815661726132764041431839102912426991427194974395769462431410735199955905343985824903');
  Priv.PrimeKey2.SetDec('8573695372469847739125271816213638824277647604717881633559571969225574325225015620887808957893360658142977333484565099818973457402882454768789495637432799');
  Priv.PrivKeyProduct.SetDec('102254192880968838810801382868389181256002534349148386664790879432128469476008930592645952145670063316387886310707201792269823813875471904969765005471'
    + '655784895966560585402535313253239266974336591272564426699631037042766443337890644687331420209218848611174173514131044514861915637897961332996331736169243193497');
  Priv.PrivKeyExponent.SetDec('39672511136982081334852781820872662950644314523029678314931072391484666575621085465907939411446247157266807837135542059783707067998407374882375067429'
    + '509912283003943601853564564341100084349650097087748956228566192837448482334604835364484274776995406202376013966890951221884425618589294581045684761888524743389');
  Priv.UpdateCRT;

  Pub.PubKeyExponent.SetDec('65537');
  Pub.PubKeyProduct.SetDec('10225419288096883881080138286838918125600253434914838666479087943212846947600893059264595214567006331638788631070720179226982381387547190496976500547165'
    + '5784895966560585402535313253239266974336591272564426699631037042766443337890644687331420209218848611174173514131044514861915637897961332996331736169243193497');

  Data := TCnBigNumber.Create;
  Data.SetDec('45019174914781748823493609390109834987678278891945905092634530145872347');

  E := TCnBigNumber.Create;
  CnRSAEncrypt(Data, Priv, E);

  Result := E.ToDec = '96747283652558959300179125936102841654752510162366056606338104428515065097089222419042825041387371477775830734475095159590586274367556207334722571658137582409'
    + '455112588244253820143045858882020011804795404413066466940159780215793973867511667474510838891278022915995230636767948103885920804051739764679233657405';
  if not Result then Exit;

  D := TCnBigNumber.Create;
  CnRSADecrypt(D, Pub, E);
  Result := D.ToDec = Data.ToDec;

  D.Free;
  E.Free;
  Data.Free;
  Pub.Free;
  Priv.Free;
end;

function TestRSAPrivPubPkcs1: Boolean;
const
  PEM = '-----BEGIN RSA PRIVATE KEY-----' + SCRLF +
    'MIIEowIBAAKCAQEAx/WjnDhkoGxhA98dT43fCQneQMzfLhwcMNU693cLcU7VlJRn' + SCRLF +
    'JPRb04IZYn1kOuretPvMmDCDHv8r4hZxaue0IW6sUjfSgcf2kk70WT+0NMHlSjWl' + SCRLF +
    'h3R56rePbbDyQRJZJTZS85iki49VjoWO4JUzyH4iyPto8uu6/gHdmR/bfaEMxb9W' + SCRLF +
    'VFWENHzeznCk8+84tWzi6Chom8sryKBxaaa3b8SsfBAfutgFqNWtc69xWb5k3xqU' + SCRLF +
    'tyFVLX9Ga9cG07lsW0Zsnw5zHNS9wQi7MIrNlfPVhsWr943rjAyea0UnXzhFQfRH' + SCRLF +
    'GuHbNKaQTsoiDlZXfYCq4vQ/TWTcuJrflBmsBQIDAQABAoIBAQCUeCIcO48TwoUi' + SCRLF +
    'T8a+rBN/7ZDVwoiv/vU7mQeoeP7JCgTxxmLzgHCyEjZw97O1P3FPJmtaUSL6n/Lg' + SCRLF +
    'c1ORUitga4GNpp8p9+Rv7CnoHrHaPmHWgRDAC87+ZX8crjah9FK2m6hp8Nu0OCAJ' + SCRLF +
    'q8dTn9UMOAPyASKCxF9afN4h9RKPKvaHI9Em0+1i9Y5MIRv5MYDhRC+9cxj3ExFX' + SCRLF +
    'ZyAtV+IQkYIGaUEEZiE3yFhITlBQyI+W9nY/tdP9UaNwHvj4gNff9QZs2aWUcgbp' + SCRLF +
    'uvqSzHegJpeG8wBBIi/1OAZh0lNg4isImcXcxbmaLfUTl2tcMn2bY5A54q/M97EG' + SCRLF +
    'P9OnMVTdAoGBAOPXJwE1MlcvUzogmEl0DPQUMOztGkeqEjUNgHgXrndQBnsN2fqS' + SCRLF +
    '2ILeZkRq0XsdEnEM5e3mt8zvLnm63IvYC14/P1gKXM24PbXMARYnQtDZR/4TJC9+' + SCRLF +
    '1mg/mydTaDuDslww2J6gT1avJXZ4nA9A/FXG6c0kyZrDZ8COZdsW2OhTAoGBAOCs' + SCRLF +
    'Vm7gI0u1H7RAwi6ndtnVxOLL0DNbPsWyb8yvMpS/jMTqJWBejX5ko6vtu8dTPdW4' + SCRLF +
    'iEMDob8Q/AmWXIhGXqPZ5MGguaafnSnT+JTGqZG3f6g4y6gKoA3aOWWTROV8SEpD' + SCRLF +
    'TvW4tflcbKJ9lCqdbPp9nO8bTka/779WOyGWTi9HAoGAWLMoQjYdlQedlSGfZ+lA' + SCRLF +
    'Kb26lmJwOrohQHozK428pEKJY1qvI/gLRlei5LvZNd7JkS1+TjHmGGh9ZXKz7dV8' + SCRLF +
    'B4MD/Bh8rLIvhBkZyzAZuQj+GBIHol30xr0MgDDegVzyESYLDyZF/O9O3GJ0DxQz' + SCRLF +
    'qIk5+F18Bz5PpA1L5hFs/HUCgYBzlpZ9QplGGPAvt2VvxT5zQfSiTgNWeKop4ex8' + SCRLF +
    '3OHj/0x3/pKuKtcW4MYH7S4/lUbpzp9kAtlTSHSypgYDIlkMaUy0hXEeFV4xJ9nr' + SCRLF +
    'CO4yhrqJnHbChHzlw5Dl/kdi3Xb/pfNYnegZW4CUdJEm/4kSsk9sAJtb8OFyifWw' + SCRLF +
    'CPk+fwKBgCJc9FM7r2I78I8JZcb13EK42NRuMKrbQxA9MVOgImEWRv/XlQpRbAvM' + SCRLF +
    'FjSgW48MX1FYg1iEwU1yWVlEn8i8o2qNOiewCmwPyBIqlNXzB+Yjl3/fMVSAKSSj' + SCRLF +
    'rZxpGqzDLndn4shtB74jrTDU+uOu8RLQhRbQVAE8TlQOusLXjY/p' + SCRLF +
    '-----END RSA PRIVATE KEY-----';
var
  S, D: AnsiString;
  Sl: TStringList;
  Stream: TMemoryStream;
  Priv: TCnRSAPrivateKey;
  Pub: TCnRSAPublicKey;
begin
  Stream := TMemoryStream.Create;
  S := AnsiString(PEM);
  Stream.Write(S[1], Length(S));
  Stream.Position := 0;

  Priv := TCnRSAPrivateKey.Create;
  Pub := TCnRSAPublicKey.Create;

  Result := CnRSALoadKeysFromPem(Stream, Priv, Pub);
  if not Result then Exit;

  Result := CnRSAVerifyKeys(Priv, Pub);
  if not Result then Exit;

  Stream.Size := 0;
  Result := CnRSASaveKeysToPem(Stream, Priv, Pub);
  if not Result then Exit;

  Stream.Position := 0;
  Sl := TStringList.Create;
  Sl.LoadFromStream(Stream);

  D := Trim(AnsiString(Sl.Text));
  Result := S = D;

  Sl.Free;
  Pub.Free;
  Priv.Free;
  Stream.Free;
end;

function TestRSAPubPkcs1: Boolean;
const
  PEM =
    '-----BEGIN RSA PUBLIC KEY-----' + SCRLF +
    'MIIBCgKCAQEAx/WjnDhkoGxhA98dT43fCQneQMzfLhwcMNU693cLcU7VlJRnJPRb' + SCRLF +
    '04IZYn1kOuretPvMmDCDHv8r4hZxaue0IW6sUjfSgcf2kk70WT+0NMHlSjWlh3R5' + SCRLF +
    '6rePbbDyQRJZJTZS85iki49VjoWO4JUzyH4iyPto8uu6/gHdmR/bfaEMxb9WVFWE' + SCRLF +
    'NHzeznCk8+84tWzi6Chom8sryKBxaaa3b8SsfBAfutgFqNWtc69xWb5k3xqUtyFV' + SCRLF +
    'LX9Ga9cG07lsW0Zsnw5zHNS9wQi7MIrNlfPVhsWr943rjAyea0UnXzhFQfRHGuHb' + SCRLF +
    'NKaQTsoiDlZXfYCq4vQ/TWTcuJrflBmsBQIDAQAB' + SCRLF +
    '-----END RSA PUBLIC KEY-----';
var
  S, D: AnsiString;
  Sl: TStringList;
  Stream: TMemoryStream;
  Pub: TCnRSAPublicKey;
begin
  Stream := TMemoryStream.Create;
  S := AnsiString(PEM);
  Stream.Write(S[1], Length(S));
  Stream.Position := 0;

  Pub := TCnRSAPublicKey.Create;

  Result := CnRSALoadPublicKeyFromPem(Stream, Pub);
  if not Result then Exit;

  Stream.Size := 0;
  Result := CnRSASavePublicKeyToPem(Stream, Pub, CnRSA.cktPKCS1);
  if not Result then Exit;

  Stream.Position := 0;
  Sl := TStringList.Create;
  Sl.LoadFromStream(Stream);

  D := Trim(AnsiString(Sl.Text));
  Result := S = D;

  Sl.Free;
  Pub.Free;
  Stream.Free;
end;

function TestRSAPrivPubPkcs8: Boolean;
const
  PEM =
    '-----BEGIN PRIVATE KEY-----' + SCRLF +
    'MIIEvQIBADANBgkqhkiG9w0BAQEFAASCBKcwggSjAgEAAoIBAQDH9aOcOGSgbGED' + SCRLF +
    '3x1Pjd8JCd5AzN8uHBww1Tr3dwtxTtWUlGck9FvTghlifWQ66t60+8yYMIMe/yvi' + SCRLF +
    'FnFq57QhbqxSN9KBx/aSTvRZP7Q0weVKNaWHdHnqt49tsPJBElklNlLzmKSLj1WO' + SCRLF +
    'hY7glTPIfiLI+2jy67r+Ad2ZH9t9oQzFv1ZUVYQ0fN7OcKTz7zi1bOLoKGibyyvI' + SCRLF +
    'oHFpprdvxKx8EB+62AWo1a1zr3FZvmTfGpS3IVUtf0Zr1wbTuWxbRmyfDnMc1L3B' + SCRLF +
    'CLswis2V89WGxav3jeuMDJ5rRSdfOEVB9Eca4ds0ppBOyiIOVld9gKri9D9NZNy4' + SCRLF +
    'mt+UGawFAgMBAAECggEBAJR4Ihw7jxPChSJPxr6sE3/tkNXCiK/+9TuZB6h4/skK' + SCRLF +
    'BPHGYvOAcLISNnD3s7U/cU8ma1pRIvqf8uBzU5FSK2BrgY2mnyn35G/sKegesdo+' + SCRLF +
    'YdaBEMALzv5lfxyuNqH0UrabqGnw27Q4IAmrx1Of1Qw4A/IBIoLEX1p83iH1Eo8q' + SCRLF +
    '9ocj0SbT7WL1jkwhG/kxgOFEL71zGPcTEVdnIC1X4hCRggZpQQRmITfIWEhOUFDI' + SCRLF +
    'j5b2dj+10/1Ro3Ae+PiA19/1BmzZpZRyBum6+pLMd6Aml4bzAEEiL/U4BmHSU2Di' + SCRLF +
    'KwiZxdzFuZot9ROXa1wyfZtjkDnir8z3sQY/06cxVN0CgYEA49cnATUyVy9TOiCY' + SCRLF +
    'SXQM9BQw7O0aR6oSNQ2AeBeud1AGew3Z+pLYgt5mRGrRex0ScQzl7ea3zO8uebrc' + SCRLF +
    'i9gLXj8/WApczbg9tcwBFidC0NlH/hMkL37WaD+bJ1NoO4OyXDDYnqBPVq8ldnic' + SCRLF +
    'D0D8VcbpzSTJmsNnwI5l2xbY6FMCgYEA4KxWbuAjS7UftEDCLqd22dXE4svQM1s+' + SCRLF +
    'xbJvzK8ylL+MxOolYF6NfmSjq+27x1M91biIQwOhvxD8CZZciEZeo9nkwaC5pp+d' + SCRLF +
    'KdP4lMapkbd/qDjLqAqgDdo5ZZNE5XxISkNO9bi1+Vxson2UKp1s+n2c7xtORr/v' + SCRLF +
    'v1Y7IZZOL0cCgYBYsyhCNh2VB52VIZ9n6UApvbqWYnA6uiFAejMrjbykQoljWq8j' + SCRLF +
    '+AtGV6Lku9k13smRLX5OMeYYaH1lcrPt1XwHgwP8GHyssi+EGRnLMBm5CP4YEgei' + SCRLF +
    'XfTGvQyAMN6BXPIRJgsPJkX8707cYnQPFDOoiTn4XXwHPk+kDUvmEWz8dQKBgHOW' + SCRLF +
    'ln1CmUYY8C+3ZW/FPnNB9KJOA1Z4qinh7Hzc4eP/THf+kq4q1xbgxgftLj+VRunO' + SCRLF +
    'n2QC2VNIdLKmBgMiWQxpTLSFcR4VXjEn2esI7jKGuomcdsKEfOXDkOX+R2Lddv+l' + SCRLF +
    '81id6BlbgJR0kSb/iRKyT2wAm1vw4XKJ9bAI+T5/AoGAIlz0UzuvYjvwjwllxvXc' + SCRLF +
    'QrjY1G4wqttDED0xU6AiYRZG/9eVClFsC8wWNKBbjwxfUViDWITBTXJZWUSfyLyj' + SCRLF +
    'ao06J7AKbA/IEiqU1fMH5iOXf98xVIApJKOtnGkarMMud2fiyG0HviOtMNT6467x' + SCRLF +
    'EtCFFtBUATxOVA66wteNj+k=' + SCRLF +
    '-----END PRIVATE KEY-----';
var
  S, D: AnsiString;
  Sl: TStringList;
  Stream: TMemoryStream;
  Priv: TCnRSAPrivateKey;
  Pub: TCnRSAPublicKey;
begin
  Stream := TMemoryStream.Create;
  S := AnsiString(PEM);
  Stream.Write(S[1], Length(S));
  Stream.Position := 0;

  Priv := TCnRSAPrivateKey.Create;
  Pub := TCnRSAPublicKey.Create;

  Result := CnRSALoadKeysFromPem(Stream, Priv, Pub);
  if not Result then Exit;

  Result := CnRSAVerifyKeys(Priv, Pub);
  if not Result then Exit;

  Stream.Size := 0;
  Result := CnRSASaveKeysToPem(Stream, Priv, Pub, CnRSA.cktPKCS8);
  if not Result then Exit;

  Stream.Position := 0;
  Sl := TStringList.Create;
  Sl.LoadFromStream(Stream);

  D := Trim(AnsiString(Sl.Text));
  Result := S = D;

  Pub.Free;
  Priv.Free;
  Stream.Free;
end;

function TestRSAPubPkcs8: Boolean;
const
  PEM =
    '-----BEGIN PUBLIC KEY-----' + SCRLF +
    'MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAx/WjnDhkoGxhA98dT43f' + SCRLF +
    'CQneQMzfLhwcMNU693cLcU7VlJRnJPRb04IZYn1kOuretPvMmDCDHv8r4hZxaue0' + SCRLF +
    'IW6sUjfSgcf2kk70WT+0NMHlSjWlh3R56rePbbDyQRJZJTZS85iki49VjoWO4JUz' + SCRLF +
    'yH4iyPto8uu6/gHdmR/bfaEMxb9WVFWENHzeznCk8+84tWzi6Chom8sryKBxaaa3' + SCRLF +
    'b8SsfBAfutgFqNWtc69xWb5k3xqUtyFVLX9Ga9cG07lsW0Zsnw5zHNS9wQi7MIrN' + SCRLF +
    'lfPVhsWr943rjAyea0UnXzhFQfRHGuHbNKaQTsoiDlZXfYCq4vQ/TWTcuJrflBms' + SCRLF +
    'BQIDAQAB' + SCRLF +
    '-----END PUBLIC KEY-----';
var
  S, D: AnsiString;
  Sl: TStringList;
  Stream: TMemoryStream;
  Pub: TCnRSAPublicKey;
begin
  Stream := TMemoryStream.Create;
  S := AnsiString(PEM);
  Stream.Write(S[1], Length(S));
  Stream.Position := 0;

  Pub := TCnRSAPublicKey.Create;

  Result := CnRSALoadPublicKeyFromPem(Stream, Pub);
  if not Result then Exit;

  Stream.Size := 0;
  Result := CnRSASavePublicKeyToPem(Stream, Pub);
  if not Result then Exit;

  Stream.Position := 0;
  Sl := TStringList.Create;
  Sl.LoadFromStream(Stream);

  D := Trim(AnsiString(Sl.Text));
  Result := S = D;

  Sl.Free;
  Pub.Free;
  Stream.Free;
end;

function TestChameleonHash: Boolean;
var
  Num, Prime, Root, Hash, SecKey, Rand, NewNum, NewRand, NewHash: TCnBigNumber;
begin
  Num := TCnBigNumber.Create;
  Prime := TCnBigNumber.Create;
  Root := TCnBigNumber.Create;
  Hash := TCnBigNumber.Create;
  SecKey := TCnBigNumber.Create;
  Rand := TCnBigNumber.Create;
  NewNum := TCnBigNumber.Create;
  NewRand := TCnBigNumber.Create;
  NewHash := TCnBigNumber.Create;

  try
    Num.SetDec('73618513111136280958052434894378447995623');
    Prime.SetDec('276970540154943142069819073642413600499');
    Root.SetDec('2');
    SecKey.SetDec('7934806187680192847293875645476892163274087655');
    Rand.SetDec('87341342568764456767764521100906219000756');

    Result := CnChameleonHashCalcDigest(Num, Rand, SecKey, Hash, Prime, Root);
    if not Result then Exit;

    Result := Hash.ToDec = '205504075662966566109157397043723549962';
    if not Result then Exit;

    NewNum.SetDec('961397411368986421346864224325900985312468');
    Result := CnChameleonHashFindRandom(Num, NewNum, Rand, SecKey, NewRand, Prime, Root);
    if not Result then Exit;

    Result := NewRand.ToDec = '50644656753223154550407161304306599524';
    if not Result then Exit;

    Result := CnChameleonHashCalcDigest(NewNum, NewRand, SecKey, NewHash, Prime, Root);
    if not Result then Exit;

    Result := BigNumberEqual(Hash, NewHash);
  finally
    Num.Free;
    Prime.Free;
    Root.Free;
    Hash.Free;
    SecKey.Free;
    Rand.Free;
    NewNum.Free;
    NewHash.Free;
  end;
end;

function TestRSA2Crypt: Boolean;
const
  KEY1 =
    '-----BEGIN RSA PRIVATE KEY-----' + #13#10 +
    'MIIEowIBAAKCAQEAxOiG2ZD8KB6njUiPJeLD8JmP+9QGqxpIQ9o94LPrAqxXFDpt' + #13#10 +
    'zpgAw3rQfvcAZaoZkgCFOYFWXtPVk0Oy7/NZ27SGQO4hKvlIAFjrcqVvoXJey6SC' + #13#10 +
    'EB/Q169ePoy3voG49ZXXlvSrfaf2cuPbosnS2vk3C18492AazmY1jCRbQVxDXQcF' + #13#10 +
    'jv0/YE3bSozydNvIvPyaFFPdWjJxSr5Hja4QbE+TR+DVRGtA/tGkLafR6rNrp6BL' + #13#10 +
    'rKXFkoCq0seXhKLvmKF7y/fy+NXT+RI34e7dz+PVMu7RYRZIIipAMumL+QvQxCp0' + #13#10 +
    'Cm1Ant48m4es2gI0/yVmD++YBmlHcFbPAtR9awIDAQABAoIBAGO4dwKcIHeY/rXY' + #13#10 +
    'd1Zyf1TMEFUyzaW9i7eBQTEZLP2PlhISfSXRaSGWgxyprrPN72E3jkDPNZSUp2cL' + #13#10 +
    'NAW9MlbIF+2uK3H+CO7UGXlYOy6CI2vyhkPwOO3iTFJVJYD/ZVJKboJuqqLaez84' + #13#10 +
    'EjVhDL4E1FGYCduN+kVpEdlFWEnCsnjW/YoxvNZ0BDHMJE8wTHWfULiIdEq4pjxT' + #13#10 +
    'DqiJ98SP124OYqx89nC0GZQ0V+byE5vWlm66v5jQmRvmhySMXmLM7AM91Zummurb' + #13#10 +
    'QtS8oGA2TzHyXEQ/CoKLrGyg+5nF6foOifgx0zDBWRzvhvGeMHr9IsxKR44q9NM2' + #13#10 +
    'W5OZqcECgYEA6Dem/SqUcilNSIgST8s/2A6S03JBp799eDnmbZUVs50LxWtRaGuX' + #13#10 +
    'ks+FlZJLrtzROm+jTQ7eBJjQzALomnNKADaf7pzjigBHMmxFnisad8iCHnAlK3zr' + #13#10 +
    'vL8gvgFL5S1myvdCn2ENjY4zdJ+iPsXrkionR6d8sGMIKGy1mV5cuBECgYEA2RMh' + #13#10 +
    'QpYuKH2xzSpLsTb4q6ocaQMIMPuXq8HF8wbMSQN4eD8HKKPyvewUlO/horZujVxg' + #13#10 +
    'oZXbgy5KDqE5gJGm6HKIctGsqVFxKZFw8IiZ5LAgBInaPr8CbiBVDyw4n2p2ZApE' + #13#10 +
    'fxn81juQR56H9ngih2z9w/+qNZ1nywgr3up6ebsCgYBwhWb5DYTYvIKiPq0A1S+e' + #13#10 +
    'dZFXu+lsazFU7Flnh/H4EoT9qD7OJjRQAxZrn3Pky0Lm2el7EVUrTRD/iflDvdGB' + #13#10 +
    'wPZGHOd0myXknOou9hvhJttF/HlGVUW1M7ed2er4pcNFXgJ+T/zNNrZgMGnhmO3I' + #13#10 +
    '6XwXEGUu4w206Ngl9L9gwQKBgCwPO/L99IR3br1L1m0z0SlWr8mIugLnLhPIktsP' + #13#10 +
    'CCvRroQJlvRiwoRWBJ9uSQfzq2C53Usu1Y08uf9aLgewiIYpqRRVBoyfYS6kvJ21' + #13#10 +
    'vDa0oOsK5+dQcbfUjC82NI/21ezcQKbjqXP6RwCiZspZ+/gs4R0FKZEUT9rf37ex' + #13#10 +
    'NAa9AoGBAMLCrQutRbpESFd2YCUnVpR/XZVPwPzyGJEO2XIjNQGJRbkL/SCw3iOq' + #13#10 +
    '4mHXBJbd2/v/arJ6KOOiPMvgmyjHjCAIeC0INMCyyFbeO0YuF9xdKQmX+12V4oZU' + #13#10 +
    'n5pVIVJmV56F7+wjcOWpvBZJKjm1fRHi7B9/eQc7J5GZDQu3oksp' + #13#10 +
    '-----END RSA PRIVATE KEY-----';
  KEY2 =
    '-----BEGIN RSA PRIVATE KEY-----' + #13#10 +
    'MIIEowIBAAKCAQEA2Q5FvKydE9LW9TzV0hGNaOtfuP4xYdmXM1bGjsgt+TI34efM' + #13#10 +
    'xj+YeqKIihkujfEuidWk8TnaHN+/ojUso73w0ma7u2m9wH/xAlcQtTaLH6c/ALIv' + #13#10 +
    'XqtPjXTtbQo91J+Zx30Mt/M/GNCGbcIwQPrMnuqxnb26mvcZW0MR/neVs+g0w0nn' + #13#10 +
    '6BgnGTd2bUNcXmNpJQmkSx/j71Z0kvCDT+lTv0UjOKniOySHNb5YyzFAIUmyLMjP' + #13#10 +
    'BTgHO8NoCsyO4avhZ8KtSb9i3lQNxeW4lwHPgIOzWaktn760o+skhuTr7HTgLbsa' + #13#10 +
    'DqRPztHfPKBdT8HepIeEo//GT4drcH41S4mLswIDAQABAoIBAGGFRRXZJBXVA9lZ' + #13#10 +
    'RORGGJfMMMzIAF3rSkC5uypJjEZLJsprwBhOWG64+cm6OK1zcCpjf+EV0gZpDQuP' + #13#10 +
    'Aohq/Xk6yRwSDTwg/6LChXI1mFpXZTol1JyfMXXn1AjKsi8Gqivz5jP7qRy4C73i' + #13#10 +
    'opV/WVIJNlYd+WpInO7g+oOvLOaQGIVyJGckk1AbNSZapbUygh+Xh3CfelqsZIY6' + #13#10 +
    'bWEufCApGuikTozjiy5B9mGwOC6gNZg0zzgMRrwwTIhsy2uiKKLXtGrw4gxykdwe' + #13#10 +
    'u6Le8//+eNmf5ocBzUOeJorEHz9TzNvI2R/d/C8++fNriVMZrPZ408q+n6DhCcBz' + #13#10 +
    'gp8pqLkCgYEA+VHMxpQxGAg3LUgGHVx8U0pyuNO/25djvLampl7hYJzNMV/Cxy55' + #13#10 +
    'eQfCRsyEckaV0VXgSmNKneiKskIILtx2yKLGmQO5rEF210ULKZPpA+fQr4KoUMtw' + #13#10 +
    'JrC4a4D+THtf+tfuKRLEhs+x2LrYTOsu8mQAFcOG5JDPitSuKWnR0QUCgYEA3t8o' + #13#10 +
    '+M09GeS3l2xFvWMnYnvD4sMaI+uPXj6GQZa1XHELY321KiZKAqlcyZWFvghBuMpe' + #13#10 +
    'JYc27mcdDmx7+KkG12XgSlQLHM0chW6Hq6U89Jt8arwNuFCI+hxPLWmm7d4zqbSW' + #13#10 +
    'Re2vLyskboDUYJClCn03WUjDj8EqCzbsVi0g51cCgYABtUbC4YcIlB3UqJaqItUT' + #13#10 +
    'OQJaM9EvrvTW+SVhJGtE3y4ktXQ/KxlX8+6nz4Tkx4kFOyJjQBTlyg8RM5ScxZPm' + #13#10 +
    'wf68U6M1A+nNhcKS0VmaVj8+xJVkoFAvY6yPx/12Bq5cqJgQHfnMtqWQR49tz3qt' + #13#10 +
    '3HOHsqQ88qUJXXWoAPqYBQKBgGEQ8mhCAw2G9orFhi4wxBN5cdwOOxFP47YZJyHj' + #13#10 +
    'wINNXLdtJX0BBhTYrGfDbmdQc0dHI+/WTw9P63C/wNKQ00D2xtO4fMqDbpuSgxY3' + #13#10 +
    'ti+WlH5r/tG6iZ3cvIM7048fyoJr+1LjbPvvH0PdaiHQfYDu/i8tqLawW15dAluZ' + #13#10 +
    '9g2rAoGBANqi8uPZCuefXl0lqN/gWQdBjPuZcM/NIs17O/dOIdgqybKAPTF3pnN3' + #13#10 +
    'kMzXixFpr0gA/I595Ey/Kqtbms0nZnhf4DKxUY99FW930WzE5hXJbtolqEtndup5' + #13#10 +
    'tLLb5C+2g8f0qIARkPHeAkBMVKdFOaFB22wXVDuZOsIeaBTM/shV' + #13#10 +
    '-----END RSA PRIVATE KEY-----';
var
  S: AnsiString;
  Stream: TMemoryStream;
  Priv1, Priv2: TCnRSAPrivateKey;
  Pub1, Pub2: TCnRSAPublicKey;
  C1, C2, En, D1, D2: TBytes;
begin
  Result := False;

  Priv1 := TCnRSAPrivateKey.Create;
  Priv2 := TCnRSAPrivateKey.Create;
  Pub1 := TCnRSAPublicKey.Create;
  Pub2 := TCnRSAPublicKey.Create;

  Stream := TMemoryStream.Create;
  try
    S := AnsiString(KEY1);
    Stream.Write(S[1], Length(S));
    Stream.Position := 0;

    if not CnRSALoadKeysFromPem(Stream, Priv1, Pub1) then
      Exit;

    S := AnsiString(KEY2);
    Stream.Size := 0;
    Stream.Write(S[1], Length(S));
    Stream.Position := 0;

    if not CnRSALoadKeysFromPem(Stream, Priv2, Pub2) then
      Exit;

    C1 := AnsiToBytes('Test Encrypt Message 1.');
    C2 := AnsiToBytes('Message 2 For Test.');

    En := CnRSA2EncryptBytes(C1, C2, Pub1, Pub2);
    if Length(En) <= 0 then
      Exit;

    D1 := CnRSA2DecryptBytes(En, Priv1);
    D2 := CnRSA2DecryptBytes(En, Priv2);

    Result := CompareBytes(D1, C1) and CompareBytes(D2, C2);
  finally
    Pub2.Free;
    Pub1.Free;
    Priv2.Free;
    Priv1.Free;
  end;
end;

function TestRSALongStream1: Boolean;
const
  KEY =
    '-----BEGIN RSA PRIVATE KEY-----' + #13#10 +
    'MIICXQIBAAKBgQCGVs2u3xXdEwQum3Um/8/IP7FcfkkaYpnHwO1EW84L0lzD8qSF' + #13#10 +
    'bWFc2ou2IZ133VcXtRbUfZEonzD8ljYTdGXuVS9rqRlJ2JXTWFI0jWy+9UQBW8hU' + #13#10 +
    'T+ak3eKNGEk9tRy+gniike74qrclUmjuKgv7zTdt+N/z7CIGRmVWvC/B3QIDAQAB' + #13#10 +
    'AoGAF/0yN5MAvXyi14vNLMyrlw/ApUqr1TlcSq5p8DYQok3LYPZYaLcylrk0D68L' + #13#10 +
    'BpeQ8NvWmtVdcYqT3dcZCvpTJSpQCzRewWfiCPYjKoJ/0wIhTMedQLCOw8joOeop' + #13#10 +
    '2LFq3ui1vsbnst5oktw6GJFNHxvJ4NpgbNnieqQkSWxkMqUCQQD16RKsh2rgNNaa' + #13#10 +
    'L1tN2+9cXexucFfNulcMaK0FSmx2ylJdJcI7evgpvnPcYcgXobvmeHYxVty7ZHX7' + #13#10 +
    'OTkQxgRTAkEAi9nWbdwj/jAZxEe3G/JAczOsY8A8FKLmutXiYmfoHl9deh4lxpgw' + #13#10 +
    'Dp9fb2adUwSWuWDIew4r5iPyoP0NDURbDwJBAIU5JP3FS3h2B8F2YH+45F9lHv7h' + #13#10 +
    '7B+vkRNO7lWMcWCV0bNXDnhM8X8kB/7gFpf+7h45KscmKOV40pYs9SaKMLMCQHhy' + #13#10 +
    'eVfNDcLSsp52FaKgFhoiGwseeaBcXNP1ejC+xQ/DmsKeTHKqiFlPseZEPqNNhHLM' + #13#10 +
    'hF5Xaj+gHkvBJgiTIskCQQCp0FMAgOCBQdsJKYxEetiBb2DxdTiam6s1QA8xcQ99' + #13#10 +
    'I0FMSVba8467OWrg6DyOMyp5NfIy4Gci7+8CuGAMvcGr' + #13#10 +
    '-----END RSA PRIVATE KEY-----';
  PLAIN1 = 'A Test Message Single.'; // 一块内
  PLAIN2 = '123456789012345678901234567890' + #13#10 +
    '123456789012345678901234567890' + #13#10 +
    '123456789012345678901234567890' + #13#10 +
    '123456789012345678901234567890' + #13#10; // 一块整

  PLAIN3 = '123456789012345678901234567890' + #13#10 +
    '123456789012345678901234567890' + #13#10 +
    '123456789012345678901234567890' + #13#10 +
    '123456789012345678901234567890' + #13#10 +
    'abcdefghijklmnopqrst'; // 一块多

  PLAIN4 = '123456789012345678901234567890' + #13#10 +
    '123456789012345678901234567890' + #13#10 +
    '123456789012345678901234567890' + #13#10 +
    '123456789012345678901234567890' + #13#10 +
    '123456789012345678901234567890' + #13#10 +
    '123456789012345678901234567890' + #13#10 +
    '123456789012345678901234567890' + #13#10 +
    '123456789012345678901234567890' + #13#10; // 两块整
var
  Priv: TCnRSAPrivateKey;
  Pub: TCnRSAPublicKey;
  InStream, OutStream: TMemoryStream;
  Plain, DecB: TBytes;
begin
  Result := False;

  Priv := TCnRSAPrivateKey.Create;
  Pub := TCnRSAPublicKey.Create;
  InStream := TMemoryStream.Create;
  OutStream := TMemoryStream.Create;

  try
    CnRSALoadKeysFromPemStr(KEY, Priv, Pub);

    Plain := AnsiToBytes(PLAIN1);
    BytesToStream(Plain, InStream);
    InStream.Position := 0;
    OutStream.Size := 0;
    if CnRSAEncryptLongStream(InStream, OutStream, Pub) then
    begin
      OutStream.Position := 0;
      InStream.Size := 0;
      if CnRSADecryptLongStream(OutStream, InStream, Priv) then
      begin
        DecB := StreamToBytes(InStream);
        Result := CompareBytes(Plain, DecB);
      end;
    end;

    if not Result then Exit;

    Plain := AnsiToBytes(PLAIN2);
    BytesToStream(Plain, InStream);
    InStream.Position := 0;
    OutStream.Size := 0;
    if CnRSAEncryptLongStream(InStream, OutStream, Pub) then
    begin
      OutStream.Position := 0;
      InStream.Size := 0;
      if CnRSADecryptLongStream(OutStream, InStream, Priv) then
      begin
        DecB := StreamToBytes(InStream);
        Result := CompareBytes(Plain, DecB);
      end;
    end;

    if not Result then Exit;

    Plain := AnsiToBytes(PLAIN3);
    BytesToStream(Plain, InStream);
    InStream.Position := 0;
    OutStream.Size := 0;
    if CnRSAEncryptLongStream(InStream, OutStream, Pub) then
    begin
      OutStream.Position := 0;
      InStream.Size := 0;
      if CnRSADecryptLongStream(OutStream, InStream, Priv) then
      begin
        DecB := StreamToBytes(InStream);
        Result := CompareBytes(Plain, DecB);
      end;
    end;

    if not Result then Exit;

    Plain := AnsiToBytes(PLAIN4);
    BytesToStream(Plain, InStream);
    InStream.Position := 0;
    OutStream.Size := 0;
    if CnRSAEncryptLongStream(InStream, OutStream, Pub) then
    begin
      OutStream.Position := 0;
      InStream.Size := 0;
      if CnRSADecryptLongStream(OutStream, InStream, Priv) then
      begin
        DecB := StreamToBytes(InStream);
        Result := CompareBytes(Plain, DecB);
      end;
    end;
  finally
    OutStream.Free;
    InStream.Free;
    Pub.Free;
    Priv.Free;
  end;
end;

function TestRSALongStream2: Boolean;
const
  KEY =
    '-----BEGIN RSA PRIVATE KEY-----' + #13#10 +
    'MIICXQIBAAKBgQCGVs2u3xXdEwQum3Um/8/IP7FcfkkaYpnHwO1EW84L0lzD8qSF' + #13#10 +
    'bWFc2ou2IZ133VcXtRbUfZEonzD8ljYTdGXuVS9rqRlJ2JXTWFI0jWy+9UQBW8hU' + #13#10 +
    'T+ak3eKNGEk9tRy+gniike74qrclUmjuKgv7zTdt+N/z7CIGRmVWvC/B3QIDAQAB' + #13#10 +
    'AoGAF/0yN5MAvXyi14vNLMyrlw/ApUqr1TlcSq5p8DYQok3LYPZYaLcylrk0D68L' + #13#10 +
    'BpeQ8NvWmtVdcYqT3dcZCvpTJSpQCzRewWfiCPYjKoJ/0wIhTMedQLCOw8joOeop' + #13#10 +
    '2LFq3ui1vsbnst5oktw6GJFNHxvJ4NpgbNnieqQkSWxkMqUCQQD16RKsh2rgNNaa' + #13#10 +
    'L1tN2+9cXexucFfNulcMaK0FSmx2ylJdJcI7evgpvnPcYcgXobvmeHYxVty7ZHX7' + #13#10 +
    'OTkQxgRTAkEAi9nWbdwj/jAZxEe3G/JAczOsY8A8FKLmutXiYmfoHl9deh4lxpgw' + #13#10 +
    'Dp9fb2adUwSWuWDIew4r5iPyoP0NDURbDwJBAIU5JP3FS3h2B8F2YH+45F9lHv7h' + #13#10 +
    '7B+vkRNO7lWMcWCV0bNXDnhM8X8kB/7gFpf+7h45KscmKOV40pYs9SaKMLMCQHhy' + #13#10 +
    'eVfNDcLSsp52FaKgFhoiGwseeaBcXNP1ejC+xQ/DmsKeTHKqiFlPseZEPqNNhHLM' + #13#10 +
    'hF5Xaj+gHkvBJgiTIskCQQCp0FMAgOCBQdsJKYxEetiBb2DxdTiam6s1QA8xcQ99' + #13#10 +
    'I0FMSVba8467OWrg6DyOMyp5NfIy4Gci7+8CuGAMvcGr' + #13#10 +
    '-----END RSA PRIVATE KEY-----';
  PLAIN1 = 'A Test Message Single.'; // 一块内
  PLAIN2 = '123456789012345678901234567890' + #13#10 +
    '123456789012345678901234567890' + #13#10 +
    '123456789012345678901234567890' + #13#10 +
    '123456789012345678901234567890' + #13#10; // 一块整

  PLAIN3 = '123456789012345678901234567890' + #13#10 +
    '123456789012345678901234567890' + #13#10 +
    '123456789012345678901234567890' + #13#10 +
    '123456789012345678901234567890' + #13#10 +
    'abcdefghijklmnopqrst'; // 一块多

  PLAIN4 = '123456789012345678901234567890' + #13#10 +
    '123456789012345678901234567890' + #13#10 +
    '123456789012345678901234567890' + #13#10 +
    '123456789012345678901234567890' + #13#10 +
    '123456789012345678901234567890' + #13#10 +
    '123456789012345678901234567890' + #13#10 +
    '123456789012345678901234567890' + #13#10 +
    '123456789012345678901234567890' + #13#10; // 两块整
var
  Priv: TCnRSAPrivateKey;
  Pub: TCnRSAPublicKey;
  InStream, OutStream: TMemoryStream;
  Plain, DecB: TBytes;
begin
  Result := False;

  Priv := TCnRSAPrivateKey.Create;
  Pub := TCnRSAPublicKey.Create;
  InStream := TMemoryStream.Create;
  OutStream := TMemoryStream.Create;

  try
    CnRSALoadKeysFromPemStr(KEY, Priv, Pub);

    Plain := AnsiToBytes(PLAIN1);
    BytesToStream(Plain, InStream);
    InStream.Position := 0;
    OutStream.Size := 0;
    if CnRSAEncryptLongStream(InStream, OutStream, Priv) then
    begin
      OutStream.Position := 0;
      InStream.Size := 0;
      if CnRSADecryptLongStream(OutStream, InStream, Pub) then
      begin
        DecB := StreamToBytes(InStream);
        Result := CompareBytes(Plain, DecB);
      end;
    end;

    if not Result then Exit;

    Plain := AnsiToBytes(PLAIN2);
    BytesToStream(Plain, InStream);
    InStream.Position := 0;
    OutStream.Size := 0;
    if CnRSAEncryptLongStream(InStream, OutStream, Priv) then
    begin
      OutStream.Position := 0;
      InStream.Size := 0;
      if CnRSADecryptLongStream(OutStream, InStream, Pub) then
      begin
        DecB := StreamToBytes(InStream);
        Result := CompareBytes(Plain, DecB);
      end;
    end;

    if not Result then Exit;

    Plain := AnsiToBytes(PLAIN3);
    BytesToStream(Plain, InStream);
    InStream.Position := 0;
    OutStream.Size := 0;
    if CnRSAEncryptLongStream(InStream, OutStream, Priv) then
    begin
      OutStream.Position := 0;
      InStream.Size := 0;
      if CnRSADecryptLongStream(OutStream, InStream, Pub) then
      begin
        DecB := StreamToBytes(InStream);
        Result := CompareBytes(Plain, DecB);
      end;
    end;

    if not Result then Exit;

    Plain := AnsiToBytes(PLAIN4);
    BytesToStream(Plain, InStream);
    InStream.Position := 0;
    OutStream.Size := 0;
    if CnRSAEncryptLongStream(InStream, OutStream, Priv) then
    begin
      OutStream.Position := 0;
      InStream.Size := 0;
      if CnRSADecryptLongStream(OutStream, InStream, Pub) then
      begin
        DecB := StreamToBytes(InStream);
        Result := CompareBytes(Plain, DecB);
      end;
    end;
  finally
    OutStream.Free;
    InStream.Free;
    Pub.Free;
    Priv.Free;
  end;
end;

// ================================ KDF ========================================

function TestKDFPB1: Boolean;
var
  Pass, Salt, Res: AnsiString;
  P, S, R: TBytes;
begin
  P := AnsiToBytes('123456');
  S := HexToBytes('123456');
  R := CnPBKDF1Bytes(P, S, 1000, 16, cpdfMd5);
  Result := DataToHex(@R[0], Length(R)) = '090583F4EA468E822CDC7A8C7C785E1B';
  if not Result then Exit;

  Pass := '123456';
  Salt := HexToAnsiStr('123456');
  Res := CnPBKDF1(Pass, Salt, 1000, 16, cpdfMd5);
  Result := DataToHex(@Res[1], Length(Res)) = '090583F4EA468E822CDC7A8C7C785E1B';
end;

function TestKDFPB2: Boolean;
var
  Pass, Salt, Res: AnsiString;
  P, S, R: TBytes;
begin
  P := AnsiToBytes('123456');
  S := HexToBytes('123456');
  R := CnPBKDF2Bytes(P, S, 1000, 32, cpdfSha256Hmac);
  Result := DataToHex(@R[0], Length(R)) = '87410D487A6414E9ADB9D078CBA7E28BFCB0C3767F1BD4C1A628010FF91DDD1A';
  if not Result then Exit;

  Pass := '123456';
  Salt := HexToAnsiStr('123456');
  Res := CnPBKDF2(Pass, Salt, 1000, 32, cpdfSha256Hmac);
  Result := DataToHex(@Res[1], Length(Res)) = '87410D487A6414E9ADB9D078CBA7E28BFCB0C3767F1BD4C1A628010FF91DDD1A';
end;

function TestKDFSM2SM9: Boolean;
var
  Pass, Res, PB: TBytes;
  P, S1, S2, S3: AnsiString;
  I: Integer;
begin
  for I := 8 to 1000 do
  begin
    P := 'CnPack';
    S1 := AnsiStrToHex(CnSM2KDF(P, I));
    S2 := AnsiStrToHex(CnSM9KDF(@P[1], Length(P), I));
    PB := AnsiToBytes(P);
    S3 := AnsiString(BytesToHex(CnSM2SM9KDF(PB, I)));
    Result := (S1 = S2) and (S2 = S3);
    if not Result then Exit;
  end;

  Pass := HexToBytes('57E7B63623FAE5F08CDA468E872A20AFA03DED41BF1403770E040DC83AF31A67991F2B01EBF9EFD8881F0A0493000603');
  Res := CnSM2SM9KDF(Pass, 19);
  Result := DataToHex(@Res[0], Length(Res)) = '046B04A9ADF53B389B9E2AAFB47D90F4D08978';
end;

function TestHKDF: Boolean;
var
  IKM, Salt, Info, Key: TBytes;
begin
  IKM := AnsiToBytes('input master key');
  Salt := AnsiToBytes('salt');
  Info := AnsiToBytes('context info');

  Key := CnHKDFBytes(chkSha256, IKM, Salt, Info, 32);
  Result := DataToHex(@Key[0], Length(Key)) = '97BE21FE349CD0AE15BC2AF067E0CC017FA11811A65EBA68D2D3EA9D8E4B9F1B';

  if not Result then Exit;

  Key := CnHKDFBytes(chkMd5, IKM, Salt, Info, 32);
  Result := DataToHex(@Key[0], Length(Key)) = 'F828FB1619735FE4AE5BA177938AABDCFDE3F7D9BE8D7F450635AF2D175E2DA1';

  if not Result then Exit;

  Key := CnHKDFBytes(chkSha1, IKM, Salt, Info, 32);
  Result := DataToHex(@Key[0], Length(Key)) = 'BE1BB878F64762BD739E0D177F4A6C27B352446614A95EEBA7D908313AB5DFE5';

  if not Result then Exit;

  Key := CnHKDFBytes(chkSha3_256, IKM, Salt, Info, 32);
  Result := DataToHex(@Key[0], Length(Key)) = '5F4CC820246E4AFEAB56E94E5795BC606A1594D55F8296661C6420E26BCB4371';

  if not Result then Exit;

  IKM := AnsiToBytes('input keying material');
  Info := AnsiToBytes('optional context string');
  Key := CnHKDFBytes(chkSm3, IKM, nil, Info, 32); // Salt 为空
  Result := DataToHex(@Key[0], Length(Key)) = '040E1C83AB8CA2C2AD73EDAE1EA6025DD21F77AA1AA088926D3843BCA70310CA';
end;

// ================================ Prime Number ===============================

function TestPrimeNumber1: Boolean;
begin
  Result := CnInt64IsPrime($E838B1A3989C4CED) and CnInt64AKSIsPrime(7347991325871728837)
    and CnInt64IsPerfectPower(1350851717672992089) and (CnInt64BigStepGiantStep(8723, 3623, 65537) = 21200)
    and CnIsInt64PrimitiveRoot($AF5C45392648247B, $AF5C453926482478)
    and CnIsInt64PrimitiveRoot($A3841E9214A5C2C7, 7);
end;

function TestPrimeNumber2: Boolean;
var
  R, F: array of TUInt64;
  C: TUInt64;
begin
  SetLength(R, 3);
  SetLength(F, 3);

  // 有物不知其数，三三数之剩二，五五数之剩三，七七数之剩二。问物几何？
  F[0] := 3; F[1] := 5; F[2] := 7;
  R[0] := 2; R[1] := 3; R[2] := 2;
  C := ChineseRemainderTheoremInt64(R, F);
  Result := C = 23;
end;

function TestPrimeNumber3: Boolean;
begin
  Result := CnInt64IsPerfectPower(9682651996416);  // 42 的 8 次方
end;

function TestPrimeNumber4: Boolean;
begin
  // 雅可比符号计算
  Result := CnInt64JacobiSymbol(17, 101) = 1;
  if not Result then Exit;

  Result := CnInt64JacobiSymbol(15, 21) = 0; // 不互素，0
  if not Result then Exit;

  Result := CnInt64JacobiSymbol(8419, 68073) = -1;
  if not Result then Exit;

  Result := CnInt64JacobiSymbol(14147, 68756437) = 1;
  if not Result then Exit;

  Result := CnInt64JacobiSymbol(-1, 7) = -1;
  if not Result then Exit;

  Result := CnInt64JacobiSymbol(-3, 19) = 1;
  if not Result then Exit;

  Result := CnInt64JacobiSymbol(-7, -37) = -1;
  if not Result then Exit;

  Result := CnInt64JacobiSymbol(-15, -21) = 0;
end;

function TestPrimeNumber5: Boolean;
var
  F32: TCnUInt32List;
  F64: TCnUInt64List;
begin
  // 素数因数分解
  F32 := TCnUInt32List.Create;
  CnUInt32FindFactors(0, F32);
  F32.IntSort;
  Result := F32.ToString = '';
  if not Result then Exit;

  F32.Clear;
  CnUInt32FindFactors(1, F32);
  F32.IntSort;
  Result := F32.ToString = '1';
  if not Result then Exit;

  F32.Clear;
  CnUInt32FindFactors(2, F32);
  F32.IntSort;
  Result := F32.ToString = '2';
  if not Result then Exit;

  F32.Clear;
  CnUInt32FindFactors(240, F32);
  F32.IntSort;
  Result := F32.ToString = '2,2,2,2,3,5';
  if not Result then Exit;
  F32.Free;

  F64 := TCnUInt64List.Create;
  CnInt64FindFactors(0, F64);
  F64.IntSort;
  Result := F64.ToString = '';
  if not Result then Exit;

  F64.Clear;
  CnInt64FindFactors(1, F64);
  F64.IntSort;
  Result := F64.ToString = '1';
  if not Result then Exit;

  F64.Clear;
  CnInt64FindFactors(2, F64);
  F64.IntSort;
  Result := F64.ToString = '2';
  if not Result then Exit;

  F64.Clear;
  CnInt64FindFactors(24086086400, F64);
  F64.IntSort;
  Result := F64.ToString = '2,2,2,2,2,2,2,2,5,5,1609,2339';
  if not Result then Exit;

  F64.Free;
end;

function TestSquareRoot: Boolean;
var
  R: Int64;
begin
  R := CnInt64SquareRoot(4, 37); // 8 余 5 型
  Result := R = 35;

  if not Result then Exit;

  R := CnInt64SquareRoot(5, 11); // 4 余 3 型
  Result := R = 4;

  if not Result then Exit;

  R := CnInt64SquareRoot(8, 17); // 8 余 1 型，注意有两个解
  Result := (R = 5) or (R = 12);

  if not Result then Exit;

  R := CnInt64SquareRoot(9, 25); // 非奇素数，无解
  Result := R = 0;

  if not Result then Exit;

  R := CnInt64SquareRoot(20, 25); // 不互素，无解
  Result := R = 0;
end;

function TestBPSWIsPrime: Boolean;
begin
  // 这些是素数
  Result := CnInt64BPSWIsPrime(2) and CnInt64BPSWIsPrime(3) and CnInt64BPSWIsPrime(5)
    and CnInt64BPSWIsPrime(859) and CnInt64BPSWIsPrime(2999) and CnInt64BPSWIsPrime(16769023)
    and CnInt64BPSWIsPrime(87178291199) and CnInt64BPSWIsPrime(9223372036854775783);

  if not Result then Exit;

  // 这些不是素数
  Result := not CnInt64BPSWIsPrime(6) and not CnInt64BPSWIsPrime(125)
    and not CnInt64BPSWIsPrime(9999999999) and not CnInt64BPSWIsPrime(87178291200)
    and not CnInt64BPSWIsPrime(24036584) and not CnInt64BPSWIsPrime(9223372036854775784);
end;

// ================================ 25519 ========================================

function Test25519CurveMul: Boolean;
var
  Curve: TCnCurve25519;
  K: TCnBigNumber;
  P: TCnEccPoint;
  D: TCnCurve25519Data;
begin
  // 测试用例来源于 RFC 7748 中的 Test Vector
  // a546e36bf0527c9d3b16154b82465edd62144c0ac1fc5a18506a2244ba449ac4 * e6db6867583030db3594c1a424b15f7c726624ec26b3353b10a903a6d0ab1c4c
  // 要 = c3da55379de9c6908e94ea4df28d084f32eccf03491c71f754b4075577a28552 后两者均为 u

  HexToData('A546E36BF0527C9D3B16154B82465EDD62144C0AC1FC5A18506A2244BA449AC4', @D[0]);
  K := TCnBigNumber.Create;
  CnCurve25519DataToBigNumber(D, K);
  CnProcess25519ScalarNumber(K);

  P := TCnEccPoint.Create;
  HexToData('E6DB6867583030DB3594C1A424B15F7C726624EC26B3353B10A903A6D0AB1C4C', @D[0]);
  CnCurve25519DataToBigNumber(D, P.X);

  Curve := TCnCurve25519.Create;
  Curve.MultiplePoint(K, P);

  CnCurve25519BigNumberToData(P.X, D);
  Result := DataToHex(@D[0], SizeOf(TCnCurve25519Data)) = 'C3DA55379DE9C6908E94EA4DF28D084F32ECCF03491C71F754B4075577A28552';

  Curve.Free;
  P.Free;
  K.Free;
end;

function Test25519CurveGMul: Boolean;
var
  Curve: TCnCurve25519;
  K: TCnBigNumber;
  P: TCnEccPoint;
  D: TCnCurve25519Data;
begin
  // 测试用例来源于 RFC 7748 中的 Diffie-Hellman 的 Test Vector
  // 77076d0a7318a57d3c16c17251b26645df4c2f87ebc0992ab177fba51db92c2a * 9
  // 要 = 8520f0098930a754748b7ddcb43ef75a0dbf3a0d26381af4eba4a98eaa9b4e6a 后两者均为 u

  HexToData('77076D0A7318A57D3C16C17251B26645DF4C2F87EBC0992AB177FBA51DB92C2A', @D[0]);
  K := TCnBigNumber.Create;
  CnCurve25519DataToBigNumber(D, K);
  CnProcess25519ScalarNumber(K);

  P := TCnEccPoint.Create;
  HexToData('0900000000000000000000000000000000000000000000000000000000000000', @D[0]);
  CnCurve25519DataToBigNumber(D, P.X);

  Curve := TCnCurve25519.Create;
  Curve.MultiplePoint(K, P);

  CnCurve25519BigNumberToData(P.X, D);
  Result := DataToHex(@D[0], SizeOf(TCnCurve25519Data)) = '8520F0098930A754748B7DDCB43EF75A0DBF3A0D26381AF4EBA4A98EAA9B4E6A';

  Curve.Free;
  P.Free;
  K.Free;
end;

function Test25519KeyExchange: Boolean;
var
  Priv1, Priv2: TCnCurve25519PrivateKey;
  Pub1, Pub2: TCnEccPublicKey;
  Key1, Key2, Key1O, Key2O: TCnEccPoint;
  D: TCnCurve25519Data;
begin
  Priv1 := nil;
  Priv2 := nil;
  Pub1 := nil;
  Pub2 := nil;
  Key1 := nil;
  Key2 := nil;
  Key1O := nil;
  Key2O := nil;

  try
    Priv1 := TCnCurve25519PrivateKey.Create;
    Priv2 := TCnCurve25519PrivateKey.Create;
    Pub1 := TCnEccPublicKey.Create;
    Pub2 := TCnEccPublicKey.Create;
    Key1 := TCnEccPoint.Create;
    Key2 := TCnEccPoint.Create;
    Key1O := TCnEccPoint.Create;
    Key2O := TCnEccPoint.Create;

    // 俩 Private Key 来源于 RFC 7748
    Priv1.LoadFromHex('77076D0A7318A57D3C16C17251B26645DF4C2F87EBC0992AB177FBA51DB92C2A');
    Priv2.LoadFromHex('5DAB087E624A8A4B79E17F8B83800EE66F3BB1292618B6FD1C2F8B27FF88E0EB');

    CnCurve25519KeyExchangeStep1(Priv1, Key1); // 第一方调用，产生 Key 1
    CnCurve25519KeyExchangeStep1(Priv2, Key2); // 另一方调用，产生 Key 2

    // Key2 给一，Key1 给另一方

    CnCurve25519KeyExchangeStep2(Priv1, Key2, Key1O); // 第一方调用，产生公有 Key 1O
    CnCurve25519KeyExchangeStep2(Priv2, Key1, Key2O); // 第一方调用，产生公有 Key 2O

    Result := CnEccPointsEqual(Key1O, Key2O);

    // RFC 中的 Secret K 是 Key1O 的 X 坐标倒过来
    if Result then
    begin
      CnCurve25519PointToData(Key1O, D);
      Result := DataToHex(@D[0], SizeOf(TCnCurve25519Data)) = '4A5D9D5BA4CE2DE1728E3BF480350F25E07E21C947D19E3376F09B3C1E161742';
    end;
  finally
    Key2O.Free;
    Key1O.Free;
    Key2.Free;
    Key1.Free;
    Pub2.Free;
    Pub1.Free;
    Priv2.Free;
    Priv1.Free;
  end;
end;

function Test25519CalcKey: Boolean;
var
  S, K: TCnBigNumber;
  D: TCnEd25519Data;
  Ed: TCnEd25519;
  Pub: TCnEd25519PublicKey;
begin
  // RFC 8032 中的 Test Vector
  // SECRET KEY: 9d61b19deffd5a60ba844af492ec2cc44449c5697b326919703bac031cae7f60
  // PUBLIC KEY: d75a980182b10ab7d54bfed3c964073a0ee172f3daa62325af021a68f707511a

  S := TCnBigNumber.Create;
  K := TCnBigNumber.Create;
  Ed := TCnEd25519.Create;
  Pub := TCnEd25519PublicKey.Create;

  HexToData('9D61B19DEFFD5A60BA844AF492EC2CC44449C5697B326919703BAC031CAE7F60', @D[0]);
  CnEd25519DataToBigNumber(D, S);
  CnCalcKeysFromEd25519PrivateKey(S, K, nil);

  Pub.Assign(Ed.Generator);
  Ed.MultiplePoint(K, Pub);

  Pub.SaveToData(D);
  Result := DataToHex(@D[0], SizeOf(TCnEd25519Data)) = 'D75A980182B10AB7D54BFED3C964073A0EE172F3DAA62325AF021A68F707511A';

  Pub.Free;
  Ed.Free;
  K.Free;
  S.Free;
end;

function Test25519Sign: Boolean;
var
  Ed: TCnEd25519;
  PrivKey: TCnEd25519PrivateKey;
  PubKey: TCnEd25519PublicKey;
  SigData: TCnEd25519SignatureData;
  Sig: TCnEd25519Signature;
  B: Byte;
begin
  // RFC 8032 中的 Test Vector 2
  Ed := TCnEd25519.Create;
  PrivKey := TCnEd25519PrivateKey.Create;
  PubKey := TCnEd25519PublicKey.Create;
  Sig := TCnEd25519Signature.Create;

  try
    PrivKey.LoadFromHex('4CCD089B28FF96DA9DB6C346EC114E0F5B8A319F35ABA624DA8CF6ED4FB8A6FB');
    PubKey.LoadFromHex('3D4017C3E843895A92B70AA74D1B7EBC9C982CCF2EC4968CC0CD55F12AF4660C');

    B := $72;
    Result := CnEd25519SignData(@B, 1, PrivKey, PubKey, Sig);
    if not Result then Exit;

    Sig.SaveToData(SigData);
    Result := DataToHex(@SigData, SizeOf(SigData)) = '92A009A9F0D4CAB8720E820B5F642540A2B27B5416503F8FB3762223EBDB69DA085AC1E43E15996E458F3613D0F11D8C387B2EAEB4302AEEB00D291612BB0C00';
    if not Result then Exit;

    Result := CnEd25519VerifyData(@B, 1, Sig, PubKey);
  finally
    Sig.Free;
    PubKey.Free;
    PrivKey.Free;
    Ed.Free;
  end;
end;

function Test448CurveMul: Boolean;
var
  Curve: TCnCurve448;
  K: TCnBigNumber;
  P: TCnEccPoint;
  D: TCnCurve448Data;
begin
  // 测试用例来源于 RFC 7748 中的 Test Vector
  // 203d494428b8399352665ddca42f9de8fef600908e0d461cb021f8c538345dd77c3e4806e25f46d3315c44e0a5b4371282dd2c8d5be3095f
  // * 0fbcc2f993cd56d3305b0b7d9e55d4c1a8fb5dbb52f8e9a1e9b6201b165d015894e56c4d3570bee52fe205e28a78b91cdfbde71ce8d157db
  // 要 = 884a02576239ff7a2f2f63b2db6a9ff37047ac13568e1e30fe63c4a7ad1b3ee3a5700df34321d62077e63633c575c1c954514e99da7c179d
  // 后两者均为 u

  FillChar(D[0], SizeOf(TCnCurve448Data), 0);
  HexToData('203D494428B8399352665DDCA42F9DE8FEF600908E0D461CB021F8C538345DD77C3E4806E25F46D3315C44E0A5B4371282DD2C8D5BE3095F', @D[0]);
  K := TCnBigNumber.Create;
  CnCurve448DataToBigNumber(D, K);
  CnProcessCurve448ScalarNumber(K);

  P := TCnEccPoint.Create;
  FillChar(D[0], SizeOf(TCnCurve448Data), 0);
  HexToData('0FBCC2F993CD56D3305B0B7D9E55D4C1A8FB5DBB52F8E9A1E9B6201B165D015894E56C4D3570BEE52FE205E28A78B91CDFBDE71CE8D157DB', @D[0]);
  CnCurve448DataToBigNumber(D, P.X);

  Curve := TCnCurve448.Create;
  Curve.MultiplePoint(K, P);

  CnCurve448BigNumberToData(P.X, D);
  Result := DataToHex(@D[0], SizeOf(TCnCurve448Data)) = '884A02576239FF7A2F2F63B2DB6A9FF37047AC13568E1E30FE63C4A7AD1B3EE3A5700DF34321D62077E63633C575C1C954514E99DA7C179D';

  Curve.Free;
  P.Free;
  K.Free;
end;

function Test448CurveGMul: Boolean;
var
  Curve: TCnCurve448;
  K: TCnBigNumber;
  P: TCnEccPoint;
  D: TCnCurve448Data;
begin
  // 测试用例来源于 RFC 7748 中的 Diffie-Hellman 的 Test Vector
  // 9a8f4925d1519f5775cf46b04b5800d4ee9ee8bae8bc5565d498c28dd9c9baf574a9419744897391006382a6f127ab1d9ac2d8c0a598726b * 5
  // 要 = 9b08f7cc31b7e3e67d22d5aea121074a273bd2b83de09c63faa73d2c22c5d9bbc836647241d953d40c5b12da88120d53177f80e532c41fa0 后两者均为 u

  FillChar(D[0], SizeOf(TCnCurve448Data), 0);
  HexToData('9A8F4925D1519F5775CF46B04B5800D4EE9EE8BAE8BC5565D498C28DD9C9BAF574A9419744897391006382A6F127AB1D9AC2D8C0A598726B', @D[0]);
  K := TCnBigNumber.Create;
  CnCurve448DataToBigNumber(D, K);
  CnProcessCurve448ScalarNumber(K);

  P := TCnEccPoint.Create;
  FillChar(D[0], SizeOf(TCnCurve448Data), 0);
  HexToData('0500000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000', @D[0]);
  CnCurve448DataToBigNumber(D, P.X);

  Curve := TCnCurve448.Create;
  Curve.MultiplePoint(K, P);

  CnCurve448BigNumberToData(P.X, D);
  Result := DataToHex(@D[0], SizeOf(TCnCurve448Data)) = '9B08F7CC31B7E3E67D22D5AEA121074A273BD2B83DE09C63FAA73D2C22C5D9BBC836647241D953D40C5B12DA88120D53177F80E532C41FA0';

  Curve.Free;
  P.Free;
  K.Free;
end;

function Test448KeyExchange: Boolean;
var
  Priv1, Priv2: TCnCurve448PrivateKey;
  Pub1, Pub2: TCnEccPublicKey;
  Key1, Key2, Key1O, Key2O: TCnEccPoint;
  D: TCnCurve448Data;
begin
  Priv1 := nil;
  Priv2 := nil;
  Pub1 := nil;
  Pub2 := nil;
  Key1 := nil;
  Key2 := nil;
  Key1O := nil;
  Key2O := nil;

  try
    Priv1 := TCnCurve448PrivateKey.Create;
    Priv2 := TCnCurve448PrivateKey.Create;
    Pub1 := TCnEccPublicKey.Create;
    Pub2 := TCnEccPublicKey.Create;
    Key1 := TCnEccPoint.Create;
    Key2 := TCnEccPoint.Create;
    Key1O := TCnEccPoint.Create;
    Key2O := TCnEccPoint.Create;

    // 俩 Private Key 来源于 RFC 7748
    Priv1.LoadFromHex('9A8F4925D1519F5775CF46B04B5800D4EE9EE8BAE8BC5565D498C28DD9C9BAF574A9419744897391006382A6F127AB1D9AC2D8C0A598726B');
    Priv2.LoadFromHex('1C306A7AC2A0E2E0990B294470CBA339E6453772B075811D8FAD0D1D6927C120BB5EE8972B0D3E21374C9C921B09D1B0366F10B65173992D');

    CnCurve448KeyExchangeStep1(Priv1, Key1); // 第一方调用，产生 Key 1
    CnCurve448KeyExchangeStep1(Priv2, Key2); // 另一方调用，产生 Key 2

    // Key2 给一，Key1 给另一方

    CnCurve448KeyExchangeStep2(Priv1, Key2, Key1O); // 第一方调用，产生公有 Key 1O
    CnCurve448KeyExchangeStep2(Priv2, Key1, Key2O); // 第一方调用，产生公有 Key 2O

    Result := CnEccPointsEqual(Key1O, Key2O);

    // RFC 中的 Secret K 是 Key1O 的 X 坐标倒过来
    if Result then
    begin
      CnCurve448PointToData(Key1O, D);
      Result := DataToHex(@D[0], SizeOf(TCnCurve448Data)) = '07FFF4181AC6CC95EC1C16A94A0F74D12DA232CE40A77552281D282BB60C0B56FD2464C335543936521C24403085D59A449A5037514A879D';
    end;
  finally
    Key2O.Free;
    Key1O.Free;
    Key2.Free;
    Key1.Free;
    Pub2.Free;
    Pub1.Free;
    Priv2.Free;
    Priv1.Free;
  end;
end;

function Test448CalcKey: Boolean;
var
  S, K: TCnBigNumber;
  D: TCnEd448Data;
  Ed: TCnEd448;
  Pub: TCnEd448PublicKey;
begin
  // RFC 8032 中的 Test Vector
  // SECRET KEY: c4eab05d357007c632f3dbb48489924d552b08fe0c353a0d4a1f00acda2c463afbea67c5e8d2877c5e3bc397a659949ef8021e954e0a12274e
  // PUBLIC KEY: 43ba28f430cdff456ae531545f7ecd0ac834a55d9358c0372bfa0c6c6798c0866aea01eb00742802b8438ea4cb82169c235160627b4c3a9480

  S := TCnBigNumber.Create;
  K := TCnBigNumber.Create;
  Ed := TCnEd448.Create;
  Pub := TCnEd448PublicKey.Create;

  HexToData('6C82A562CB808D10D632BE89C8513EBF6C929F34DDFA8C9F63C9960EF6E348A3528C8A3FCC2F044E39A3FC5B94492F8F032E7549A20098F95B', @D[0]);
  CnEd448DataToBigNumber(D, S);
  CnCalcKeysFromEd448PrivateKey(S, K, nil);

  Pub.Assign(Ed.Generator);
  Ed.MultiplePoint(K, Pub);

  Pub.SaveToData(D);
  Result := DataToHex(@D[0], SizeOf(TCnEd448Data)) = '5FD7449B59B461FD2CE787EC616AD46A1DA1342485A70E1F8A0EA75D80E96778EDF124769B46C7061BD6783DF1E50F6CD1FA1ABEAFE8256180';

  Pub.Free;
  Ed.Free;
  K.Free;
  S.Free;
end;

function Test448Sign1: Boolean;
var
  Ed: TCnEd448;
  PrivKey: TCnEd448PrivateKey;
  PubKey: TCnEd448PublicKey;
  SigData: TCnEd448SignatureData;
  Sig: TCnEd448Signature;
  B: Byte;
begin
  // RFC 8032 中的 Test Vector
  // Secret Key: c4eab05d357007c632f3dbb48489924d552b08fe0c353a0d4a1f00acda2c463afbea67c5e8d2877c5e3bc397a659949ef8021e954e0a12274e
  // Public Key: 43ba28f430cdff456ae531545f7ecd0ac834a55d9358c0372bfa0c6c6798c0866aea01eb00742802b8438ea4cb82169c235160627b4c3a9480
  // Message 1 Byte: 03
  // User Context: nil

  Ed := TCnEd448.Create;
  PrivKey := TCnEd448PrivateKey.Create;
  PubKey := TCnEd448PublicKey.Create;
  Sig := TCnEd448Signature.Create;

  try
    PrivKey.LoadFromHex('C4EAB05D357007C632F3DBB48489924D552B08FE0C353A0D4A1F00ACDA2C463AFBEA67C5E8D2877C5E3BC397A659949EF8021E954E0A12274E');
    PubKey.LoadFromHex('43BA28F430CDFF456AE531545F7ECD0AC834A55D9358C0372BFA0C6C6798C0866AEA01EB00742802B8438EA4CB82169C235160627B4C3A9480');

    B := $03;
    Result := CnEd448SignData(@B, 1, PrivKey, PubKey, Sig); // 无 UserContext
    if not Result then Exit;

    Sig.SaveToData(SigData);
    Result := DataToHex(@SigData, SizeOf(SigData)) = '26B8F91727BD62897AF15E41EB43C377EFB9C610D48F2335CB0BD0087810F4352541B143C4B981B7E18F62DE8CCDF633FC1BF037AB7CD779805E0DBCC0AAE1CBCEE1AFB2E027DF36BC04DCECBF154336C19F0AF7E0A6472905E799F1953D2A0FF3348AB21AA4ADAFD1D234441CF807C03A00';
    if not Result then Exit;

    Result := CnEd448VerifyData(@B, 1, Sig, PubKey);
  finally
    Sig.Free;
    PubKey.Free;
    PrivKey.Free;
    Ed.Free;
  end;
end;

function Test448Sign2: Boolean;
var
  Ed: TCnEd448;
  PrivKey: TCnEd448PrivateKey;
  PubKey: TCnEd448PublicKey;
  SigData: TCnEd448SignatureData;
  Sig: TCnEd448Signature;
  C: TBytes;
  B: Byte;
begin
  // RFC 8032 中的 Test Vector
  // Secret Key: c4eab05d357007c632f3dbb48489924d552b08fe0c353a0d4a1f00acda2c463afbea67c5e8d2877c5e3bc397a659949ef8021e954e0a12274e
  // Public Key: 43ba28f430cdff456ae531545f7ecd0ac834a55d9358c0372bfa0c6c6798c0866aea01eb00742802b8438ea4cb82169c235160627b4c3a9480
  // Message 1 Byte: 03
  // User Context: 666f6f

  Ed := TCnEd448.Create;
  PrivKey := TCnEd448PrivateKey.Create;
  PubKey := TCnEd448PublicKey.Create;
  Sig := TCnEd448Signature.Create;

  try
    PrivKey.LoadFromHex('C4EAB05D357007C632F3DBB48489924D552B08FE0C353A0D4A1F00ACDA2C463AFBEA67C5E8D2877C5E3BC397A659949EF8021E954E0A12274E');
    PubKey.LoadFromHex('43BA28F430CDFF456AE531545F7ECD0AC834A55D9358C0372BFA0C6C6798C0866AEA01EB00742802B8438EA4CB82169C235160627B4C3A9480');

    B := $03;
    C := HexToBytes('666F6F');
    Result := CnEd448SignData(@B, 1, PrivKey, PubKey, Sig, C); // 有 UserContext
    if not Result then Exit;

    Sig.SaveToData(SigData);
    Result := DataToHex(@SigData, SizeOf(SigData)) = 'D4F8F6131770DD46F40867D6FD5D5055DE43541F8C5E35ABBCD001B32A89F7D2151F7647F11D8CA2AE279FB842D607217FCE6E042F6815EA000C85741DE5C8DA1144A6A1ABA7F96DE42505D7A7298524FDA538FCCBBB754F578C1CAD10D54D0D5428407E85DCBC98A49155C13764E66C3C00';
    if not Result then Exit;

    Result := CnEd448VerifyData(@B, 1, Sig, PubKey, C);
  finally
    Sig.Free;
    PubKey.Free;
    PrivKey.Free;
    Ed.Free;
  end;
end;

// =============================== Paillier ====================================

function TestPaillier1: Boolean;
var
  Priv: TCnPaillierPrivateKey;
  Pub: TCnPaillierPublicKey;
  Data, EnData, RF: TCnBigNumber;
begin
  Priv := TCnPaillierPrivateKey.Create;
  Pub := TCnPaillierPublicKey.Create;
  Data := TCnBigNumber.Create;
  EnData := TCnBigNumber.Create;
  RF := TCnBigNumber.Create;

  try
    // 设置私钥参数
    Priv.P.SetDec('17865585776428015572711271647468961714224622152187158345911678'
      + '339636651092788333052027175253216775307351593118309394931434529729219410'
      + '167932230057844519390265255785152433653286504388010423310828666940276842'
      + '358638428279942977062750571502352391084960221532095223558766248563960265'
      + '0073804997664494928369855508901');
    Priv.Q.SetDec('16625416453977719514362838818137042369847927796302907576185924'
      + '522107716575003749726583509849816627789763573962790044495563071048696302'
      + '750113473747778755337680130845484241411896089406236894433296870337675699'
      + '181214790792978410682454830168065543130156392018410893342741090250858362'
      + '1401093644872290360337329030067');
    Priv.Lambda.SetDec('297022803727376641521831033536563683713993727092961929218'
      + '456824848801225120293138121948912474969565232028934714823930415547156003'
      + '445096372298005069839116682732791659990912047444868661895668331662146648'
      + '224295615492939075123243054868591593733896794727387739995641818286770727'
      + '489290995798603434352084084585228584503005204356154848793656442556301716'
      + '988061149160912695958804517069247890758749968752380192864437920479788591'
      + '246251682696300940605041228421371642403280023758709714022833987131427233'
      + '840756411298453979972011073547728077807989951610271707176418701802700823'
      + '65365320573733572145336180278376079143709329803830587400');
    Priv.Mu.SetDec('7053727228554671428651558896658014935778305049648600209165739'
      + '929914755206330397291520550163496555932470239750193186069569399126669377'
      + '208552365485722157849070415475590156030640312821722149988093277967379624'
      + '943444586841125032359674477434699674237356626033052197240703683145146138'
      + '980357925083310831111238456274295508543164409929417532082562096299305202'
      + '650555399541521415227670378693175193579375813614965640549002396638973299'
      + '298603876107991445585567543518735294154345662177298264211414145636083392'
      + '853271820543601278053694677220644102597979992189862236244989311101147532'
      + '273664103797665281035619303708770729636925343013165');

    // 设置公钥参数
    Pub.N.SetDec('297022803727376641521831033536563683713993727092961929218456824'
      + '84880122512029313812194891247496956523202893471482393041554715600344509637'
      + '22980050698391166827327916599909120474448686618956683316621466482242956154'
      + '92939075123243054868591593733896794727387739995641818286770727489290995798'
      + '60343435208408458522858795210542739672835750106748911690212539531614400991'
      + '92881685648032436846575379582466134487031677782301913052993561956253960610'
      + '18396612520225942022965607496553248377086501493649686613265488185711007707'
      + '76726522753304998510012872613081187421821212331436217870426489588806960293'
      + '522451753274721680494618511015126367');
    Pub.G.SetDec('297022803727376641521831033536563683713993727092961929218456824'
      + '84880122512029313812194891247496956523202893471482393041554715600344509637'
      + '22980050698391166827327916599909120474448686618956683316621466482242956154'
      + '92939075123243054868591593733896794727387739995641818286770727489290995798'
      + '60343435208408458522858795210542739672835750106748911690212539531614400991'
      + '92881685648032436846575379582466134487031677782301913052993561956253960610'
      + '18396612520225942022965607496553248377086501493649686613265488185711007707'
      + '76726522753304998510012872613081187421821212331436217870426489588806960293'
      + '522451753274721680494618511015126368');
    Pub.N2.SetDec('88222545934071707332157657178412586608547447737917763366823809'
      + '54335526138728022586474216399654668530678062054602135300331250656606486101'
      + '53847832101197827862000144008961671976997267607927481958136313046762531675'
      + '76291440859084077592020046358901213557694299750337942100659502413633484181'
      + '65751613444877372177498387025296444781821370558033341480717702279770495626'
      + '66147024812388813935441035441725676102952519346977225268533992936816455197'
      + '39310570039619743811031174390875968685734458568731437000921767036739520415'
      + '26162361640978608671112813633607587336985473879781583351425371523561078590'
      + '17350560410155563757357216962296412677221217447718915996195009511871872207'
      + '40850377814950640471953874101324976405086987771902418367544536055950166972'
      + '60742861786623645953201357571237454029017333663066587951836463911939032457'
      + '64103851665438204544631447170854878833087020734481302272592225103454191376'
      + '46353290922180461013824563165314493362363316807348531109941741734011703826'
      + '70213434106433577231774192949414068407580900366069639014144790471356721863'
      + '38560190863793884889147347993447980734181290393638411649296926732836812158'
      + '54358217674627952037269994752058372208173204241098932817085617061489991521'
      + '7796125402024491118683478766338215347893803244987880978618689');

    // 外界指定随机值
    RF.SetDec('21619237268730383488616681498422451490979084758669827301964595272'
      + '45096954533765260851359789405765648332749752971529007369247735880162945'
      + '13386644678921403388066672725398719960355539558661891425485400188306137'
      + '94267538406388865835201492427624862672386280044346239126407994443284989'
      + '46030591434089277851081219092606286793034174427644442133219646523537773'
      + '24718182337567236757350150435874796326929809252055404151300786546537382'
      + '14401567546588314163912091073016127047080206425460281629989425121687547'
      + '79329013433182335473816197254760469602454815327846852963826873156695508'
      + '9152090622645339790896864166216159655132076998975872410');

    // 明文
    Data.SetDec('23333');
    Result := CnPaillierEncrypt(Pub, Data, EnData, RF);
    if not Result then Exit;

    Result := EnData.ToDec = '70125285863078156806274990966621838701467358924008'
      + '94711766433527427601524327886439370664309367994942301323743564160769639'
      + '61973963781341554974910230155004818988111039190130079368461363241897883'
      + '38581242549303444657550504448093489935537553921772282387771564791227892'
      + '34775005312835028253095215787187758992181884436078203735105441646575277'
      + '02944795323656594573373909480115013177867833536391256595216329912577075'
      + '46368327271220533269714320687521234811229013946776959365374600059060717'
      + '63138605965576714312621760982811721880959317898574083654391944901507773'
      + '26663480981156040971404564697789794248434331284433223656884823439972856'
      + '78378877573854243116399601418070850851122397477121311500576617349438480'
      + '11079416502161230703210503380430494117289311393910657096876433058660270'
      + '27714967244972226101619707083742214291750257401417716665914687605752626'
      + '16596238378924909545533880712015371344123360937620537265942436391197908'
      + '86306229480731649185270116928208648964264455863534161691927434672049773'
      + '62367907364576616006306841000631515909947994783703196025596188633478085'
      + '17074145979663632540236123684981837447422276252445302540311198427738272'
      + '95253329558050753728778063882891005231794943654556648995600917786868849'
      + '41859364316206126878813392992620783677560314163';
    if not Result then Exit;

    Result := CnPaillierDecrypt(Priv, Pub, EnData, Data);
    if not Result then Exit;

    Result := Data.ToDec = '23333';
  finally
    RF.Free;
    EnData.Free;
    Data.Free;
    Pub.Free;
    Priv.Free;
  end;
end;

function TestPaillier2: Boolean;
var
  Priv: TCnPaillierPrivateKey;
  Pub: TCnPaillierPublicKey;
  Data1, Data2, Data3, Enc1, Enc2, Enc3, Dec3: TCnBigNumber;
begin
  Priv := TCnPaillierPrivateKey.Create;
  Pub := TCnPaillierPublicKey.Create;

  Data1 := BigNumberFromHex('7A9B80241F23491ECC6D');
  Data2 := BigNumberFromHex('90BCDA133FB23650C28124');
  Data3 := TCnBigNumber.Create;

  Enc1 := TCnBigNumber.Create;
  Enc2 := TCnBigNumber.Create;
  Enc3 := TCnBigNumber.Create;
  Dec3 := TCnBigNumber.Create;

  try
    Priv.P.SetDec(
      '14111036699188414870552526093763443013480411643335975947886520760569402990' +
      '57053801103735158801499186792673103187253302937381639482541041068170813214' +
      '73203931701805435291464572271923906452844175788105394616126630677100982355' +
      '33266324074633356943474763657636977807594352531588784323334706341132092018' +
      '7861911640607');
    Priv.Q.SetDec(
      '13056650540141952859790005608724184267154794633829270236052789578171110457' +
      '87707863254534915284727581011157405571463336172224482064324880094436095845' +
      '56122632610472547881658327125370942967975103832181999001478041978243697827' +
      '55520057722689304243405883599771586042166081590293574909801197148234929121' +
      '2862389419313');
    Priv.Lambda.SetDec(
      '18424287494042133659539518652832524241006333091719677734714085454404818657' +
      '40903729151756672985218068632015400836977075534196641686781764721371712049' +
      '08060430635124193216087431558363986056489777490838584199988413426796374354' +
      '56432354709261934407459524825023487147442690539148520063196513101189563580' +
      '62198313051933744073236017476159483952271567065789914093071311124046006154' +
      '60404190697424544947054740126589520094719684843006820441705772925917845136' +
      '69885162341454046393959368624355234337819284626049797473626791588507077759' +
      '66684079052992787987104906827148131813092057372377709816442459142990215017' +
      '1363910085056461579783072');
    Priv.Mu.SetDec(
      '23068129970785271516471302000826021621808611286351428448339953820708859629' +
      '73275813760965151160214624935246539600808016313487556038768808521051394214' +
      '77515492811911746304704855179627079878956677906485536976312508794991375716' +
      '98085780789169096769726018938314906635991569220901168232535717370185298181' +
      '44785079587771592008725021049016824975720037647745447386617330763480824687' +
      '04102027852190352780083688744863872506246699244793546326584900234211996691' +
      '74818856480943841580958678628796166121440138057229475265411557807770729176' +
      '38764094718261734821058654249682247534578575551790743826293527852425556624' +
      '536359617829821563655439');

    Pub.N.SetDec(
      '18424287494042133659539518652832524241006333091719677734714085454404818657' +
      '40903729151756672985218068632015400836977075534196641686781764721371712049' +
      '08060430635124193216087431558363986056489777490838584199988413426796374354' +
      '56432354709261934407459524825023487147442690539148520063196513101189563580' +
      '62198313051960911760475347843889826483974054693070549299348476370229945464' +
      '94278242042269306611413010200675746862523515351765537080815379047464711057' +
      '86145853247483372958271646607528357237216579475470616753247078982124682432' +
      '32218547071281574368902229488335012460349465936227470250564341502223350920' +
      '6257580296457185880842991');
    Pub.G.SetDec(
      '18424287494042133659539518652832524241006333091719677734714085454404818657' +
      '40903729151756672985218068632015400836977075534196641686781764721371712049' +
      '08060430635124193216087431558363986056489777490838584199988413426796374354' +
      '56432354709261934407459524825023487147442690539148520063196513101189563580' +
      '62198313051960911760475347843889826483974054693070549299348476370229945464' +
      '94278242042269306611413010200675746862523515351765537080815379047464711057' +
      '86145853247483372958271646607528357237216579475470616753247078982124682432' +
      '32218547071281574368902229488335012460349465936227470250564341502223350920' +
      '6257580296457185880842992');
    Pub.N2.SetDec(
      '33945436966311736534906258987359020017122589981085223801068303037508304345' +
      '65128680492219013550319307828551891188708822754437688676124059144008372228' +
      '92348932279502806286719598419592220679222991995386191172312762912544262468' +
      '01703012347383298684128196694610352103625237842503632573684132783059736717' +
      '66306477240240216576802644832052238478608391710514644865903069771898802732' +
      '84619711527167092339350725384544279366670872618297917146250552938807396074' +
      '81101815515146035239566055146631104854769299576283569931281629574210848828' +
      '48160372508796734080270559984223814156404577932646611302218040862776964137' +
      '59890293359143348050185292344747530663100456824901834334621286039653253669' +
      '09174783077336005996161172086214169716964082764663268419338562344603428064' +
      '23194462135657992718176284285661808006330581023035589929116527706902207494' +
      '75058432538562404589177020176952474752153764529323233573137936975559458732' +
      '45362253721741745016655344117721021565769537994085392003380014052202423939' +
      '04372896533980219519633772939618254192064433596689460043398153298978031039' +
      '58226808135508550035591156312714084610142410562687937680981946454374763756' +
      '51881271055593387912502576030878039061790361627033817102624978675153046500' +
      '7026546650804690643388427632252853565044793826081');

    Result := CnPaillierEncrypt(Pub, Data1, Enc1);
    if not Result then Exit;

    Result := CnPaillierEncrypt(Pub, Data2, Enc2);
    if not Result then Exit;

    Result := CnPaillierAddPlain(Data3, Data1, Data2, Pub);
    if not Result then Exit;

    Result := CnPaillierAddCipher(Enc3, Enc1, Enc2, Pub);
    if not Result then Exit;

    Result := CnPaillierDecrypt(Priv, Pub, Enc3, Dec3);
    if not Result then Exit;

    Result := BigNumberEqual(Dec3, Data3);
  finally
    Dec3.Free;
    Enc3.Free;
    Enc2.Free;
    Enc1.Free;
    Data3.Free;
    Data2.Free;
    Data1.Free;

    Pub.Free;
    Priv.Free;
  end;
end;

// ============================= SecretSharing =================================

function TestSecretSharingShamir: Boolean;
var
  S, P: TCnBigNumber;
  Orders, Shares, X, Y: TCnBigNumberList;
begin
  S := TCnBigNumber.FromDec('43333333333333874874874874253253');
  P := TCnBigNumber.Create;

  Orders := TCnBigNumberList.Create;
  Shares := TCnBigNumberList.Create;

  X := TCnBigNumberList.Create;
  Y := TCnBigNumberList.Create;

  try
    P.SetHex('A1E21A8374A4ED028A32195B14F6E29F2B219406015E8BF5E97B737ADF299873BA'
      + 'C0E46C60B8E2BAA6F0EB5DD9920EACFAFDACCDA31288F1C494D861A803E9FE0C056F62D'
      + '7C882EFA7D312B20C93E687715CE026BC3EC4750547EF3E375887E819B969B0F03A84D4'
      + 'B63252FDC979B952DE4C32B1BA5E8D1166DFF612EF60220B');

    Result := CnShamirSplit(S, 5, 3, Orders, Shares, P);
    if not Result then Exit;

    X.Add.SetWord(1);
    X.Add.SetWord(3);
    X.Add.SetWord(5);
    BigNumberCopy(Y.Add, Shares[0]);
    BigNumberCopy(Y.Add, Shares[2]);
    BigNumberCopy(Y.Add, Shares[4]);

    Result := CnShamirReconstruct(P, X, Y, S);
    if not Result then Exit;

    Result := S.ToHex = '0222F18D35EBF4A436D083FF4FC5';
  finally
    Y.Free;
    X.Free;
    Shares.Free;
    Orders.Free;
    P.Free;
    S.Free;
  end;
end;

function TestSecretSharingFeldmanVss: Boolean;
var
  I: Integer;
  S, P, G, O: TCnBigNumber;
  Orders, Shares, Comms, X, Y: TCnBigNumberList;
begin
  S := TCnBigNumber.FromDec('23333333333333874874874874253253');
  P := TCnBigNumber.FromHex(CN_PRIME_FFDHE_2048);
  G := TCnBigNumber.FromDec('2');

  Orders := TCnBigNumberList.Create;
  Shares := TCnBigNumberList.Create;
  Comms := TCnBigNumberList.Create;

  X := TCnBigNumberList.Create;
  Y := TCnBigNumberList.Create;
  O := TCnBigNumber.Create;

  try
    Result := CnFeldmanVssSplit(S, 5, 3, Orders, Shares, Comms, P, G);
    if not Result then Exit;

    for I := 0 to Shares.Count - 1 do
    begin
      O.SetWord(I + 1);
      Result := CnFeldmanVssVerify(P, G, O, Shares[I], Comms);
      if not Result then Exit;
    end;

    X.Add.SetWord(1);
    X.Add.SetWord(3);
    X.Add.SetWord(5);
    BigNumberCopy(Y.Add, Shares[0]);
    BigNumberCopy(Y.Add, Shares[2]);
    BigNumberCopy(Y.Add, Shares[4]);

    Result := CnFeldmanVssReconstruct(P, G, X, Y, Comms, S);
    if not Result then Exit;

    Result := S.ToHex = '01268210F5A67381A08383FF4FC5';
  finally
    O.Free;
    Y.Free;
    X.Free;
    Comms.Free;
    Shares.Free;
    Orders.Free;
    P.Free;
    S.Free;
    G.Free;
  end;
end;

// ================================ OTS ========================================

function TestOTSSM3: Boolean;
var
  Priv: TCnOTSSM3PrivateKey;
  Pub: TCnOTSSM3PublicKey;
  Sig: TCnOTSSM3Signature;
  Ver: TCnOTSSM3VerificationKey;
  S: AnsiString;
  B: TBytes;
begin
  Result := CnOTSSM3GenerateKeys(Priv, Pub);
  if not Result then Exit;

  S := 'Test Message for Hash Based One Time Signature.';
  B := AnsiToBytes(S);
  CnOTSSM3SignBytes(B, Priv, Pub, Sig, Ver);

  Result := CnOTSSM3VerifyBytes(B, Sig, Pub, Ver);
end;

function TestOTSSHA256: Boolean;
var
  Priv: TCnOTSSHA256PrivateKey;
  Pub: TCnOTSSHA256PublicKey;
  Sig: TCnOTSSHA256Signature;
  Ver: TCnOTSSHA256VerificationKey;
  S: AnsiString;
  B: TBytes;
begin
  Result := CnOTSSHA256GenerateKeys(Priv, Pub);
  if not Result then Exit;

  S := 'Test Message for Hash Based One Time Signature.';
  B := AnsiToBytes(S);
  CnOTSSHA256SignBytes(B, Priv, Pub, Sig, Ver);

  Result := CnOTSSHA256VerifyBytes(B, Sig, Pub, Ver);
end;

function TestMOTSSM3: Boolean;
var
  Priv: TCnMOTSSM3PrivateKey;
  Pub: TCnMOTSSM3PublicKey;
  Sig: TCnMOTSSM3Signature;
  S: AnsiString;
  B: TBytes;
begin
  Result := CnMOTSSM3GenerateKeys(Priv, Pub);
  if not Result then Exit;

  S := 'Test Message for Hash Based One Time Signature.';
  B := AnsiToBytes(S);
  CnMOTSSM3SignBytes(B, Priv, Sig);

  Result := CnMOTSSM3VerifyBytes(B, Sig, Pub);
end;

function TestMOTSSHA256: Boolean;
var
  Priv: TCnMOTSSHA256PrivateKey;
  Pub: TCnMOTSSHA256PublicKey;
  Sig: TCnMOTSSHA256Signature;
  S: AnsiString;
  B: TBytes;
begin
  Result := CnMOTSSHA256GenerateKeys(Priv, Pub);
  if not Result then Exit;

  S := 'Test Message for Hash Based One Time Signature.';
  B := AnsiToBytes(S);
  CnMOTSSHA256SignBytes(B, Priv, Sig);

  Result := CnMOTSSHA256VerifyBytes(B, Sig, Pub);
end;

function TestWOTSSM3: Boolean;
var
  Priv: TCnWOTSSM3PrivateKey;
  Pub: TCnWOTSSM3PublicKey;
  Sig: TCnWOTSSM3Signature;
  S: AnsiString;
  B: TBytes;
begin
  Result := CnWOTSSM3GenerateKeys(Priv, Pub);
  if not Result then Exit;

  S := 'Test Message for Hash Based One Time Signature.';
  B := AnsiToBytes(S);
  CnWOTSSM3SignBytes(B, Priv, Sig);

  Result := CnWOTSSM3VerifyBytes(B, Sig, Pub);
end;

function TestWOTSSHA256: Boolean;
var
  Priv: TCnWOTSSHA256PrivateKey;
  Pub: TCnWOTSSHA256PublicKey;
  Sig: TCnWOTSSHA256Signature;
  S: AnsiString;
  B: TBytes;
begin
  Result := CnWOTSSHA256GenerateKeys(Priv, Pub);
  if not Result then Exit;

  S := 'Test Message for Hash Based One Time Signature.';
  B := AnsiToBytes(S);
  CnWOTSSHA256SignBytes(B, Priv, Sig);

  Result := CnWOTSSHA256VerifyBytes(B, Sig, Pub);
end;

// ================================ ECC ========================================

function TestEccMul: Boolean;
const
  RES = '0408F4F37E2D8F74E18C1B8FDE2374D5F28402FB8AB7FD1CC5B786AA40851A70CBC2ECA87B8BD2C0BE52698E9D5EE19840C4D40CA696E16159134769FA1AE85B2E';
var
  Ecc: TCnEcc;
  P: TCnEccPoint;
  P3: TCnEcc3Point;
  K: TCnBigNumber;
begin
  Ecc := TCnEcc.Create(ctSecp256k1);
  P := TCnEccPoint.Create;
  P3 := TCnEcc3Point.Create;
  K := TCnBigNumber.Create;

  try
    K.SetDec('123456789');
    P.Assign(Ecc.Generator);

    Ecc.NormalMultiplePoint(K, P);
    Result := P.ToHex = RES;
    if not Result then Exit;

    P.Assign(Ecc.Generator);
    CnEccPointToEcc3Point(P, P3);
    Ecc.AffineMultiplePoint(K, P3);
    CnAffinePointToEccPoint(P3, P, Ecc.FiniteFieldSize);

    Result := P.ToHex = RES;
    if not Result then Exit;

    P.Assign(Ecc.Generator);
    CnEccPointToEcc3Point(P, P3);
    Ecc.JacobianMultiplePoint(K, P3);
    CnJacobianPointToEccPoint(P3, P, Ecc.FiniteFieldSize);
    Result := P.ToHex = RES;
  finally
    K.Free;
    P3.Free;
    P.Free;
    Ecc.Free;
  end;
end;

function TestECCPrivPubPkcs1: Boolean;
const
  PEM =
    '-----BEGIN EC PARAMETERS-----' + SCRLF +
    'BgUrgQQACg==' + SCRLF +
    '-----END EC PARAMETERS-----' + SCRLF +
    '-----BEGIN EC PRIVATE KEY-----' + SCRLF +
    'MHQCAQEEICuHh07yriJJanWerJegB55n7bE8pEDhbKNdNoegP2FnoAcGBSuBBAAK' + SCRLF +
    'oUQDQgAEbr8v5r1XGP8R1hLozBbymC0VWmYoU/N8LaouJVaFHfvBNyqaOiaDZ5/m' + SCRLF +
    'hIE7Y9kK1omjOY1Z9km9goNlVrc29A==' + SCRLF +
    '-----END EC PRIVATE KEY-----';
var
  S, D: AnsiString;
  Sl: TStringList;
  Stream: TMemoryStream;
  Priv: TCnEccPrivateKey;
  Pub: TCnEccPublicKey;
  CurveType: TCnEccCurveType;
begin
  Stream := TMemoryStream.Create;
  S := AnsiString(PEM);
  Stream.Write(S[1], Length(S));
  Stream.Position := 0;

  Priv := TCnEccPrivateKey.Create;
  Pub := TCnEccPublicKey.Create;

  Result := CnEccLoadKeysFromPem(Stream, Priv, Pub, CurveType);

  if not Result then Exit;

  Result := CnEccVerifyKeys(CurveType, Priv, Pub);

  if not Result then Exit;

  Stream.Size := 0;
  Result := CnEccSaveKeysToPem(Stream, Priv, Pub, CurveType);

  if not Result then Exit;

  Stream.Position := 0;
  Sl := TStringList.Create;
  Sl.LoadFromStream(Stream);

  D := Trim(AnsiString(Sl.Text));
  Result := S = D;

  Pub.Free;
  Priv.Free;
  Stream.Free;
end;

function TestECCPrivPubPkcs8: Boolean;
const
  PEM =
    '-----BEGIN PRIVATE KEY-----' + SCRLF +
    'MIGEAgEAMBAGByqGSM49AgEGBSuBBAAKBG0wawIBAQQgK4eHTvKuIklqdZ6sl6AH' + SCRLF +
    'nmftsTykQOFso102h6A/YWehRANCAARuvy/mvVcY/xHWEujMFvKYLRVaZihT83wt' + SCRLF +
    'qi4lVoUd+8E3Kpo6JoNnn+aEgTtj2QrWiaM5jVn2Sb2Cg2VWtzb0' + SCRLF +
    '-----END PRIVATE KEY-----';
var
  S, D: AnsiString;
  Sl: TStringList;
  Stream: TMemoryStream;
  Priv: TCnEccPrivateKey;
  Pub: TCnEccPublicKey;
  CurveType: TCnEccCurveType;
begin
  Stream := TMemoryStream.Create;
  S := AnsiString(PEM);
  Stream.Write(S[1], Length(S));
  Stream.Position := 0;

  Priv := TCnEccPrivateKey.Create;
  Pub := TCnEccPublicKey.Create;

  Result := CnEccLoadKeysFromPem(Stream, Priv, Pub, CurveType);

  if not Result then Exit;

  Result := CnEccVerifyKeys(CurveType, Priv, Pub);

  if not Result then Exit;

  Stream.Size := 0;
  Result := CnEccSaveKeysToPem(Stream, Priv, Pub, CurveType, CnECC.cktPKCS8);

  if not Result then Exit;

  Stream.Position := 0;
  Sl := TStringList.Create;
  Sl.LoadFromStream(Stream);

  D := Trim(AnsiString(Sl.Text));
  Result := S = D;

  Pub.Free;
  Priv.Free;
  Stream.Free;
end;

function TestECCPub: Boolean;
const
  PEM =
    '-----BEGIN PUBLIC KEY-----' + SCRLF +
    'MFYwEAYHKoZIzj0CAQYFK4EEAAoDQgAEbr8v5r1XGP8R1hLozBbymC0VWmYoU/N8' + SCRLF +
    'LaouJVaFHfvBNyqaOiaDZ5/mhIE7Y9kK1omjOY1Z9km9goNlVrc29A==' + SCRLF +
    '-----END PUBLIC KEY-----';
var
  S, D: AnsiString;
  Sl: TStringList;
  Stream: TMemoryStream;
  Pub: TCnEccPublicKey;
  CurveType: TCnEccCurveType;
begin
  Stream := TMemoryStream.Create;
  S := AnsiString(PEM);
  Stream.Write(S[1], Length(S));
  Stream.Position := 0;

  Pub := TCnEccPublicKey.Create;

  Result := CnEccLoadPublicKeyFromPem(Stream, Pub, CurveType);

  if not Result then Exit;

  Stream.Size := 0;
  Result := CnEccSavePublicKeyToPem(Stream, Pub, CurveType);

  if not Result then Exit;

  Stream.Position := 0;
  Sl := TStringList.Create;
  Sl.LoadFromStream(Stream);

  D := Trim(AnsiString(Sl.Text));
  Result := S = D;

  Pub.Free;
  Stream.Free;
end;

function TestECCSchoof: Boolean;
var
  A, B, Q, R: TCnBigNumber;
begin
  Result := False;

  A := TCnBigNumber.Create;
  B := TCnBigNumber.Create;
  Q := TCnBigNumber.Create;
  R := TCnBigNumber.Create;

  try
    A.SetWord(2);
    B.SetWord(1);
    Q.SetWord(13);

    if CnEccSchoof(R, A, B, Q) then
      Result := R.ToDec = '8';
    if not Result then Exit;

    A.SetWord(7);
    B.SetWord(1);
    Q.SetWord(65537);

    if CnEccSchoof(R, A, B, Q) then
      Result := R.ToDec = '65751';
    if not Result then Exit;

    A.SetWord(7);
    B.SetWord(1);
    Q.SetDec('2147483629');

    if CnEccSchoof(R, A, B, Q) then
      Result := R.ToDec = '2147464597';
    if not Result then Exit;

    A.SetWord(7);
    B.SetWord(1);
    Q.SetWord(3037000493);

    if CnEccSchoof(R, A, B, Q) then
      Result := R.ToDec = '3036927405';
    if not Result then Exit;

    A.SetWord(7);
    B.SetWord(1);
    Q.SetDec('4294967291');

    if CnEccSchoof(R, A, B, Q) then
      Result := R.ToDec = '4294994984';
    if not Result then Exit;

    A.SetWord(7);
    B.SetWord(1);
    Q.SetDec('6074000687');

    if CnEccSchoof(R, A, B, Q) then
      Result := R.ToDec = '6074024457';
    if not Result then Exit;

    A.SetWord(7);
    B.SetWord(1);
    Q.SetDec('6074001169');

    if CnEccSchoof(R, A, B, Q) then
      Result := R.ToDec = '6074123004';
  finally
    R.Free;
    Q.Free;
    B.Free;
    A.Free;
  end;
end;

function TestECCSchoof2: Boolean;
var
  A, B, Q, R: TCnBigNumber;
begin
  Result := False;

  A := TCnBigNumber.Create;
  B := TCnBigNumber.Create;
  Q := TCnBigNumber.Create;
  R := TCnBigNumber.Create;

  try
    A.SetWord(2);
    B.SetWord(1);
    Q.SetWord(13);

    if CnEccSchoof2(R, A, B, Q) then
      Result := R.ToDec = '8';
    if not Result then Exit;

    A.SetWord(7);
    B.SetWord(1);
    Q.SetWord(65537);

    if CnEccSchoof2(R, A, B, Q) then
      Result := R.ToDec = '65751';
    if not Result then Exit;

    A.SetWord(7);
    B.SetWord(1);
    Q.SetDec('2147483629');

    if CnEccSchoof2(R, A, B, Q) then
      Result := R.ToDec = '2147464597';
    if not Result then Exit;

    A.SetWord(7);
    B.SetWord(1);
    Q.SetWord(3037000493);

    if CnEccSchoof2(R, A, B, Q) then
      Result := R.ToDec = '3036927405';
    if not Result then Exit;

    A.SetWord(7);
    B.SetWord(1);
    Q.SetDec('4294967291');

    if CnEccSchoof2(R, A, B, Q) then
      Result := R.ToDec = '4294994984';
    if not Result then Exit;

    A.SetWord(7);
    B.SetWord(1);
    Q.SetDec('6074000687');

    if CnEccSchoof2(R, A, B, Q) then
      Result := R.ToDec = '6074024457';
    if not Result then Exit;

    A.SetWord(7);
    B.SetWord(1);
    Q.SetDec('6074001169');

    if CnEccSchoof2(R, A, B, Q) then
      Result := R.ToDec = '6074123004';
  finally
    R.Free;
    Q.Free;
    B.Free;
    A.Free;
  end;
end;

// ================================= END =======================================

end.
