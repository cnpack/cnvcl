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
* 软件名称：CnPack 密码算法库
* 单元名称：CnPack 密码算法库批量测试单元
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
  CnDSA, CnBLAKE, CnBLAKE2, CnXXH, CnWideStrings, CnContainers, CnMLKEM, CnMLDSA,
  CnCalendar;

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

// ================================ Calendar ======================================

function TestCalendarWeek: Boolean;
function TestCalendarYinYang: Boolean;
function TestCalendarGanZhi: Boolean;
function TestCalendarShengXiao: Boolean;
function TestCalendarXingZuo: Boolean;
function TestCalendarJieQi: Boolean;
function TestCalendar5Xing: Boolean;
function TestCalendar12Jian: Boolean;
function TestCalendar3Yuan9Yun: Boolean;
function TestCalendar9Xing: Boolean;
function TestCalendar28Xiu: Boolean;
function TestCalendar6Yao: Boolean;
function TestCalendarLunar: Boolean;
function TestCalendarShuJiu: Boolean;
function TestCalendar3Fu: Boolean;
function TestCalendarTaiShen: Boolean;
function TestCalendarJiShenFangWei: Boolean;
function TestCalendarTaiSui: Boolean;
function TestCalendarJulianDate: Boolean;
function TestCalendarSolarLunarConvert: Boolean;

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
function TestBitsCopy: Boolean;
function TestBitsBytes: Boolean;
function TestBitsTBits: Boolean;

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

// ================================ NTRU =======================================

function TestNTRUHPS2048509: Boolean;
function TestNTRUHPS2048677: Boolean;
function TestNTRUHPS4096821: Boolean;

// ================================ MLKEM ======================================

function TestMLKEM512KeyGen: Boolean;
function TestMLKEM512KeyEncapDecap: Boolean;
function TestMLKEM768KeyGen: Boolean;
function TestMLKEM768KeyEncapDecap: Boolean;
function TestMLKEM1024KeyGen: Boolean;
function TestMLKEM1024KeyEncapDecap: Boolean;

// ================================ MLDSA ======================================

function TestMLDSA44KeyGen: Boolean;
function TestMLDSA44SignVerify: Boolean;
function TestMLDSA65KeyGen: Boolean;
function TestMLDSA65SignVerify: Boolean;
function TestMLDSA87KeyGen: Boolean;
function TestMLDSA87SignVerify: Boolean;

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
function TestSHA512224: Boolean;
function TestSHA512224HMac: Boolean;
function TestSHA512224Update: Boolean;
function TestSHA512256: Boolean;
function TestSHA512256HMac: Boolean;
function TestSHA512256Update: Boolean;

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
function TestReedSolomon: Boolean;

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

// ================================ Calendar ======================================

  MyAssert(TestCalendarWeek, 'TestCalendarWeek');
  MyAssert(TestCalendarYinYang, 'TestCalendarYinYang');
  MyAssert(TestCalendarGanZhi, 'TestCalendarGanZhi');
  MyAssert(TestCalendarShengXiao, 'TestCalendarShengXiao');
  MyAssert(TestCalendarXingZuo, 'TestCalendarXingZuo');
  MyAssert(TestCalendarJieQi, 'TestCalendarJieQi');
  MyAssert(TestCalendar5Xing, 'TestCalendar5Xing');
  MyAssert(TestCalendar12Jian, 'TestCalendar12Jian');
  MyAssert(TestCalendar3Yuan9Yun, 'TestCalendar3Yuan9Yun');
  MyAssert(TestCalendar9Xing, 'TestCalendar9Xing');
  MyAssert(TestCalendar28Xiu, 'TestCalendar28Xiu');
  MyAssert(TestCalendar6Yao, 'TestCalendar6Yao');
  MyAssert(TestCalendarLunar, 'TestCalendarLunar');
  MyAssert(TestCalendarShuJiu, 'TestCalendarShuJiu');
  MyAssert(TestCalendar3Fu, 'TestCalendar3Fu');
  MyAssert(TestCalendarTaiShen, 'TestCalendarTaiShen');
  MyAssert(TestCalendarJiShenFangWei, 'TestCalendarJiShenFangWei');
  MyAssert(TestCalendarTaiSui, 'TestCalendarTaiSui');
  MyAssert(TestCalendarJulianDate, 'TestCalendarJulianDate');
  MyAssert(TestCalendarSolarLunarConvert, 'TestCalendarSolarLunarConvert');

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
  MyAssert(TestBitsCopy, 'TestBitsCopy');
  MyAssert(TestBitsBytes, 'TestBitsBytes');
  MyAssert(TestBitsTBits, 'TestBitsTBits');

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

// ================================ NTRU =======================================

  MyAssert(TestNTRUHPS2048509, 'TestNTRUHPS2048509');
  MyAssert(TestNTRUHPS2048677, 'TestNTRUHPS2048677');
  MyAssert(TestNTRUHPS4096821, 'TestNTRUHPS4096821');

// ================================ MLKEM ======================================

  MyAssert(TestMLKEM512KeyGen, 'TestMLKEM512KeyGen');
  MyAssert(TestMLKEM512KeyEncapDecap, 'TestMLKEM512KeyEncapDecap');
  MyAssert(TestMLKEM768KeyGen, 'TestMLKEM768KeyGen');
  MyAssert(TestMLKEM768KeyEncapDecap, 'TestMLKEM768KeyEncapDecap');
  MyAssert(TestMLKEM1024KeyGen, 'TestMLKEM1024KeyGen');
  MyAssert(TestMLKEM1024KeyEncapDecap, 'TestMLKEM1024KeyEncapDecap');

// ================================ MLDSA ======================================

  MyAssert(TestMLDSA44KeyGen, 'TestMLDSA44KeyGen');
  MyAssert(TestMLDSA44SignVerify, 'TestMLDSA44SignVerify');
  MyAssert(TestMLDSA65KeyGen, 'TestMLDSA65KeyGen');
  MyAssert(TestMLDSA65SignVerify, 'TestMLDSA65SignVerify');
  MyAssert(TestMLDSA87KeyGen, 'TestMLDSA87KeyGen');
  MyAssert(TestMLDSA87SignVerify, 'TestMLDSA87SignVerify');

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
  MyAssert(TestSHA512224, 'TestSHA512224');
  MyAssert(TestSHA512224HMac, 'TestSHA512224HMac');
  MyAssert(TestSHA512224Update, 'TestSHA512224Update');
  MyAssert(TestSHA512256, 'TestSHA512256');
  MyAssert(TestSHA512256HMac, 'TestSHA512256HMac');
  MyAssert(TestSHA512256Update, 'TestSHA512256Update');

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
  MyAssert(TestReedSolomon, 'TestReedSolomon');

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

// ================================ Calendar ======================================

function TestCalendarWeek: Boolean;
begin
  Result := (GetWeek(2025, 1, 1) = 3) and // 2025 年 1 月 1 日星期三
    (GetWeek(2024, 2, 29) = 4) and        // 2024 年 2 月 29 日星期四
    (GetWeek(2000, 1, 1) = 6) and         // 2000 年 1 月 1 日星期六
    (GetWeekFromNumber(0) = '日') and
    (GetWeekFromNumber(6) = '六');
end;

function TestCalendarYinYang: Boolean;
begin
  Result := (GetYinYangFromGan(0) = 1) and // 甲为阳
    (GetYinYangFromGan(1) = 0) and         // 乙为阴
    (GetYinYangFromZhi(0) = 1) and         // 子为阳
    (GetYinYangFromZhi(1) = 0) and         // 丑为阴
    (GetYinYangFromNumber(0) = '阴') and
    (GetYinYangFromNumber(1) = '阳');
end;

function TestCalendarGanZhi: Boolean;
var
  Gan, Zhi: Integer;
begin
  Result := (CombineGanZhi(0, 0) = 0) and  // 甲子
    (CombineGanZhi(9, 11) = 59) and        // 癸亥
    (ExtractGanZhi(0, Gan, Zhi) and (Gan = 0) and (Zhi = 0)) and
    (ExtractGanZhi(59, Gan, Zhi) and (Gan = 9) and (Zhi = 11)) and
    (GetTianGanFromNumber(0) = '甲') and
    (GetDiZhiFromNumber(0) = '子') and
    (GetGanZhiFromNumber(0) = '甲子') and
    (GetGanZhiFromNumber(59) = '癸亥');

  if not Result then Exit;

  Result := (GetGanZhiFromYear(2025) = 41) and // 2025 年乙巳
    (GetGanZhiFromYear(2024) = 40) and         // 2024 年甲辰
    (GetGanZhiFromMonth(2025, 1, 1) = 12) and  // 2025 年 1 月丙子
    (GetGanZhiFromDay(2025, 1, 1) = 6) and     // 2025 年 1 月 1 日庚午
    (GetGanZhiFromHour(2025, 1, 1, 0) = 12);   // 2025 年 1 月 1 日 0 时丙子
end;

function TestCalendarShengXiao: Boolean;
begin
  Result := (GetShengXiaoFromYear(2025) = 5) and // 2025 年蛇
    (GetShengXiaoFromYear(2024) = 4) and         // 2024 年龙
    (GetShengXiaoFromYear(2000) = 4) and         // 2000 年龙
    (GetShengXiaoFromNumber(0) = '鼠') and
    (GetShengXiaoFromNumber(11) = '猪');
end;

function TestCalendarXingZuo: Boolean;
begin
  Result := (GetXingZuoFromMonthDay(3, 21) = 0) and // 3 月 21 日白羊
    (GetXingZuoFromMonthDay(4, 20) = 0) and         // 4 月 20 日白羊
    (GetXingZuoFromMonthDay(4, 21) = 1) and         // 4 月 21 日金牛
    (GetXingZuoFromMonthDay(8, 23) = 5) and         // 8 月 23 日处女
    (GetXingZuoFromNumber(0) = '白羊') and
    (GetXingZuoFromNumber(11) = '双鱼');
end;

function TestCalendarJieQi: Boolean;
var
  Month, Day, Hour, Minute, Second, ActualYear: Integer;
begin
  Result := (GetJieQiFromDay(2025, 2, 3) = 0) and // 2025 年 2 月 3 日立春
    (GetJieQiFromDay(2025, 12, 21) = 21) and      // 2025 年 12 月 21 日冬至
    (GetJieQiFromDay(2025, 1, 1) = -1) and        // 2025 年 1 月 1 日不是节气
    (GetJieQiInAYear(2025, 0, Month, Day, Hour, Minute, Second, ActualYear)) and
    (Month = 1) and (Day = 5) and                 // 2025 年小寒在 1 月 5 日
    (GetJieQiFromNumber(0) = '立春') and
    (GetJieQiFromNumber(23) = '大寒');
end;

function TestCalendar5Xing: Boolean;
begin
  Result := (Get5XingFromGan(0) = 1) and  // 甲木
    (Get5XingFromGan(1) = 1) and          // 乙木
    (Get5XingFromZhi(3) = 1) and          // 卯木
    (Get5XingFromGanZhi(0, 0) = 0) and    // 甲子海中金
    (Get5XingFromDay(2025, 1, 1) = 4) and // 2025 年 1 月 1 日纳音五行土
    (Get5XingLongFromGanZhi(0, 0) = '海中金') and
    (Get5XingLongFromDay(2025, 1, 1) = '路旁土') and
    (Get5XingFromNumber(0) = '金') and
    (Get5XingFromNumber(4) = '土');
end;

function TestCalendar12Jian: Boolean;
begin
  Result := (Get12JianFromDay(2025, 1, 1) >= 0) and (Get12JianFromDay(2025, 1, 1) <= 11) and
    (Get12JianFromNumber(0) = '建') and
    (Get12JianFromNumber(11) = '闭');
end;

function TestCalendar3Yuan9Yun: Boolean;
begin
  Result := (Get3YuanFromYear(2025, 1, 1) >= 0) and (Get3YuanFromYear(2025, 1, 1) <= 2) and
    (GetYun9XingFromYear(2025, 1, 1) >= 0) and (GetYun9XingFromYear(2025, 1, 1) <= 8) and
    (Get3YuanFromNumber(0) = '上元') and
    (Get3YuanFromNumber(2) = '下元');
end;

function TestCalendar9Xing: Boolean;
begin
  Result := (Get9XingFromYear(2025, 1, 1) >= 0) and (Get9XingFromYear(2025, 1, 1) <= 8) and
    (Get9XingFromMonth(2025, 1, 1) >= 0) and (Get9XingFromMonth(2025, 1, 1) <= 8) and
    (Get9XingFromDay(2025, 1, 1) >= 0) and (Get9XingFromDay(2025, 1, 1) <= 8) and
    (Get9XingFromHour(2025, 1, 1, 12) >= 0) and (Get9XingFromHour(2025, 1, 1, 12) <= 8) and
    (Get9XingFromNumber(0) = '一白') and
    (Get9XingFromNumber(8) = '九紫');
end;

function TestCalendar28Xiu: Boolean;
begin
  Result := (Get28XiuFromDay(2025, 1, 1) >= 0) and (Get28XiuFromDay(2025, 1, 1) <= 27) and
    (GetLunar28XiuFromDay(2025, 1, 1) >= -1) and (GetLunar28XiuFromDay(2025, 1, 1) <= 27) and
    (Get28XiuFromNumber(0) = '角') and
    (Get28XiuFromNumber(27) = '轸') and
    (Get28XiuLongFromNumber(0) = '角木蛟') and
    (Get28XiuLongFromNumber(27) = '轸水蚓');
end;

function TestCalendar6Yao: Boolean;
begin
  Result := (Get6YaoFromDay(2025, 1, 1) >= 0) and (Get6YaoFromDay(2025, 1, 1) <= 5) and
    (Get6YaoFromNumber(0) = '先胜') and
    (Get6YaoFromNumber(5) = '赤口');
end;

function TestCalendarLunar: Boolean;
var
  LunarYear, LunarMonth, LunarDay: Integer;
  IsLeapMonth: Boolean;
  Year, Month, Day: Integer;
begin
  Result := GetLunarFromDay(2025, 1, 1, LunarYear, LunarMonth, LunarDay, IsLeapMonth) and
    (LunarYear = 2024) and (LunarMonth = 12) and (LunarDay = 2) and not IsLeapMonth and
    GetDayFromLunar(2024, 12, 2, False, Year, Month, Day) and
    (Year = 2025) and (Month = 1) and (Day = 1);

  if not Result then Exit;

  Result := (GetLunarMonthFromNumber(1, False) = '正月') and
    (GetLunarMonthFromNumber(1, True) = '闰正月') and
    (GetLunarDayFromNumber(1) = '初一') and
    (GetLunarDayFromNumber(15) = '十五') and
    (GetLunarDayFromNumber(30) = '三十');
end;

function TestCalendarShuJiu: Boolean;
var
  JiuSeq, JiuDay: Integer;
begin
  Result := (GetShu9Day(2025, 1, 1, JiuSeq, JiuDay)) and // 2025 年 1 月 1 日在数九内
    (JiuSeq >= 1) and (JiuSeq <= 9) and
    (JiuDay >= 1) and (JiuDay <= 9);
end;

function TestCalendar3Fu: Boolean;
var
  FuSeq, FuDay: Integer;
begin
  Result := not Get3FuDay(2025, 1, 1, FuSeq, FuDay) and // 2025年 1 月 1 日不在三伏内
    (Get3FuFromNumber(0) = '初伏') and
    (Get3FuFromNumber(1) = '中伏') and
    (Get3FuFromNumber(2) = '末伏');
end;

function TestCalendarTaiShen: Boolean;
var
  TaiShen1, TaiShen2: string;
begin
  Result := GetTaiShenStringFromDay(2025, 1, 1, TaiShen1, TaiShen2) and
    (TaiShen1 <> '') and (TaiShen2 <> '') and
    (GetTaiShenStringFromDay(2025, 1, 1) <> '');
end;

function TestCalendarJiShenFangWei: Boolean;
begin
  Result := (GetCaiShenFangWeiFromDay(2025, 1, 1) >= 0) and (GetCaiShenFangWeiFromDay(2025, 1, 1) <= 7) and
    (GetXiShenFangWeiFromDay(2025, 1, 1) >= 0) and (GetXiShenFangWeiFromDay(2025, 1, 1) <= 7) and
    (GetFuShenFangWeiFromDay(2025, 1, 1) >= 0) and (GetFuShenFangWeiFromDay(2025, 1, 1) <= 7) and
    (GetGuiShenFangWeiFromDay(2025, 1, 1) >= 0) and (GetGuiShenFangWeiFromDay(2025, 1, 1) <= 7) and
    (GetJiShenFangWeiFromNumber(0) = '正北') and
    (GetJiShenFangWeiFromNumber(7) = '西北');
end;

function TestCalendarTaiSui: Boolean;
begin
  Result := (Get12TaiSuiFromNumber(0) = '太岁') and
    (Get12TaiSuiFromNumber(11) = '病符') and
    (Get60TaiSuiFromNumber(0) = '金辨') and
    (Get60TaiSuiFromNumber(59) = '虞程');
end;

function TestCalendarJulianDate: Boolean;
var
  Year, Month, Day: Integer;
  JD, MJD: Extended;
begin
  JD := GetJulianDate(2025, 1, 1);
  MJD := GetModifiedJulianDate(2025, 1, 1);
  Result := (JD > 2400000) and (MJD > 40000) and
    GetDayFromJulianDate(JD, Year, Month, Day) and
    (Year = 2025) and (Month = 1) and (Day = 1) and
    GetDayFromModifiedJulianDate(MJD, Year, Month, Day) and
    (Year = 2025) and (Month = 1) and (Day = 1);
end;

function TestCalendarSolarLunarConvert: Boolean;
var
  LunarYear, LunarMonth, LunarDay: Integer;
  IsLeapMonth: Boolean;
  Year, Month, Day: Integer;
begin
  // 测试几个特殊日期的转换
  Result := GetLunarFromDay(2024, 2, 10, LunarYear, LunarMonth, LunarDay, IsLeapMonth) and
    GetDayFromLunar(LunarYear, LunarMonth, LunarDay, IsLeapMonth, Year, Month, Day) and
    (Year = 2024) and (Month = 2) and (Day = 10);

  if not Result then Exit;

  Result := GetLunarFromDay(2025, 1, 28, LunarYear, LunarMonth, LunarDay, IsLeapMonth) and
    GetDayFromLunar(LunarYear, LunarMonth, LunarDay, IsLeapMonth, Year, Month, Day) and
    (Year = 2025) and (Month = 1) and (Day = 28);
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

function TestBitsCopy: Boolean;
var
  B: TCnBitBuilder;
  Value: Cardinal;
begin
  // 注意这里注释中的二进制位，最右边是最低位以便于阅读，不同于 TCnBitBuilder.ToString
  B := TCnBitBuilder.Create;

  // 测试复制 8 位
  B.AppendByte($EA); // 11101010
  Value := B.Copy(0, 8);
  Result := Value = $EA;
  if not Result then Exit;

  B.Clear;
  B.AppendByte($EA, False);
  Value := B.Copy(0, 6);
  Result := Value = $2A; // 11101010 的低 6 位再高位补 0，00101010
  if not Result then Exit;

  // 测试复制中间的位
  B.Clear;
  B.AppendWord($9F3B);   // 1001111100111011
  Value := B.Copy(4, 8); //     |      |
  Result := Value = $F3; // 11110011
  if not Result then Exit;

  // 测试复制跨越字节的位
  B.Clear;
  B.AppendDWord($12345678, False); // 00010010001101000101011001111000
  Value := B.Copy(10, 16);         //       |              |
  Result := Value = $8D15;         // 1000110100010101
  if not Result then Exit;

  B.Free;
end;

function TestBitsBytes: Boolean;
var
  B: TCnBitBuilder;
  Bytes: TBytes;
  Expected: TBytes;
begin
  B := TCnBitBuilder.Create;

  // 测试 ToBytes 和 SetBytes
  B.AppendByte($EA);
  B.AppendWord($9F3B);
  Bytes := B.ToBytes;

  SetLength(Expected, 3);
  Expected[0] := $EA;
  Expected[1] := $3B;
  Expected[2] := $9F;

  Result := CompareBytes(Bytes, Expected);
  if not Result then Exit;

  // 测试 SetBytes
  B.Clear;
  B.SetBytes(Expected);
  Result := (B.BitLength = 24) and (B.ByteLength = 3);
  if not Result then Exit;

  Result := (B.Copy(0, 8) = $EA) and (B.Copy(8, 8) = $3B) and (B.Copy(16, 8) = $9F);
  if not Result then Exit;

  // 测试不完整的字节
  B.Clear;
  B.AppendByteRange($EA, 5); // 只添加 6 位 101010
  Bytes := B.ToBytes;
  Result := (Length(Bytes) = 1) and (Bytes[0] = $2A);
  if not Result then Exit;

  B.Free;
end;

function TestBitsTBits: Boolean;
var
  B: TCnBitBuilder;
  Bits: TBits;
  I: Integer;
begin
  B := TCnBitBuilder.Create;
  Bits := TBits.Create;

  B.AppendByte($EA);
  B.AppendWord($9F3B);
  B.ToBits(Bits);

  Result := (Bits.Size = 24) and
    (not Bits[0]) and Bits[1] and (not Bits[2]) and Bits[3] and     // A
    (not Bits[4]) and Bits[5] and Bits[6] and Bits[7] and           // E
    Bits[8] and Bits[9] and (not Bits[10]) and Bits[11] and         // B
    Bits[12] and Bits[13] and not (Bits[14]) and (not Bits[15]) and // 3
    Bits[16] and Bits[17] and Bits[18] and Bits[19] and             // F
    Bits[20] and (not Bits[21]) and (not Bits[22]) and Bits[23];    // 9
  if not Result then Exit;

  // 测试 TBits 到 BitBuilder
  Bits.Size := 12;
  for I := 0 to 11 do
    Bits[I] := (I mod 3) = 0; // 每 3 位设为True

  B.Clear;
  B.SetBits(Bits);

  Result := (B.BitLength = 12) and (B.ByteLength = 2);
  if not Result then Exit;

  // 验证位模式
  for I := 0 to 11 do
  begin
    if B[I] <> ((I mod 3) = 0) then
    begin
      Result := False;
      Exit;
    end;
  end;

  // 测试空 Bits
  B.Clear;
  Bits.Size := 0;
  B.SetBits(Bits);
  Result := (B.BitLength = 0);

  B.Free;
  Bits.Free;
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

// ================================ NTRU =======================================

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

// ================================ MLKEM ======================================

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

// ================================ MLDSA ======================================

function TestMLDSA44KeyGen: Boolean;
const
  S_SK =
    'D712599A161ECD99EF5B7A04313D5507D612565F03AA9695ED7C2DF1CFA180565499F298E7845116' +
    '1F0E9FF4B862815F3D5D33393AF3E677EE2460DF7BF3AE6E9A80F12A9B349DFDB9B3D0513BFFE620' +
    '394A80A2883BCC69F1847057AC3BF35E63A72E60D5267D606C524AB847BCBC1113874755248DFBE5' +
    '83A2F0DB0FCBC8CD9C9008128028CA962CA038680919464C008ACC084D00466093002A541631D9A8' +
    '90D21200DCA8515418655C2005C8306A0C01204B040E1C913001418C11448C1A3206CC060C50B270' +
    '58244593A0119822012426260B402603090E5A12020301611A14010119625222200CA3650B09904A' +
    'B44D80264D5A366263906D1207320339491036111223041888202421880AC82819806C492629C492' +
    '2062188993367204220282086558945018B32823335263A20112498C91B428E4C645DB82694B0211' +
    '01270A09397220314042A44DD23092913444C134091946200C210D4B361010261114376994408953' +
    '08204324019B464152B8854286891B11880C41449C3252D2200520320C13948510930582C6480211' +
    '865B262CCC884444B42DD2088E601062CCA66D8344451B3765C8B05180066922040CA40489514424' +
    'A3404162965019C78160320D9816921220101915016248051C362E59308A13126AA3444610A52900' +
    'C98C93C4100BA04508170558486201086C183392DCB86DCCB08D59906420026014B309104945A3A6' +
    '0012472E1C005203C91003492C1C92811B396613465001294650208462106900255043B8201CC68D' +
    '981284A03204220821DBC46CA10622A09071E4204D213984D4B44023432401B6001A318289188209' +
    '067112B131C3B624CA126C90066201036C0825909B3669E28044982428C4B0100B384159986D4B22' +
    '0118382618332E11150C49A8088C48898BC62862864C00461224A681C3C681E0C60D19046E9C9441' +
    'C1283203466E1321251CB5854AA084A424319CB02048946014B7404814501319691AB00012B12413' +
    'C880628410C31061D94040DB4806898869119289149700C204241CB7058422284B3646130132CB40' +
    '6160140CE0005218010A50228118278013329004269110A32C99B42C5C44464B3632DC468A49B048' +
    '58884961C44102A2650302624326410B9885A11426481092DA962898C80060281043866883422544' +
    '12102343511C326A09884511C5654010065BA001CC32889922258A1666420611898241120165C314' +
    '0621446C09450149424C99948C0045022C7B04EF5AD7D8FD0179FC8AFDA5DCADA47CF7926345F6CD' +
    '5F072625345A613C92D8D1EA7CFD213DFCD7896567194767DA47E3B3BED4E3F2CF727C78F7FD6385' +
    '5CC68EFEA538728CB803830B6FC646A865B93A46CB4EE30968C8A61D913DACC0483CC19DD790ACEC' +
    '8E07BA8F5E924D7F024B69552ECEB588A94DC2BF11BE97A59A8065043525C628B59CE423CCF8293C' +
    '78363C98662EE883F8B46A243AB2AB8ED3EB24C4AFDF383B2E9F0F969D4E6DE19B06BF6837ABDF75' +
    '1725B97D45389138EDB253A78C78DE841224FD7CFFAE2D8D58FF5D9B9ECD9C209F77CCA5D167B1A9' +
    '3EC5A9B891E1E4DBF1741DA1C4AAE7622B15F040DA03350CECF2649E309D38A8D5F8AA218900F321' +
    '557C593DCB83A6A5A2C3C13129A07236E6F50864E4E9E11B840F3D6E6B74ACFE7394DAD62167E1CA' +
    'E165B9207596B1C7CD3DE62558C24723C8F74BB4203F0F5272D0E1E8EC631BEC8ED97E9DC58FC843' +
    'A901329D566A3A230D138B3AAC9B29CCDBEFDD2A6AAA7AC2C57A4850AB0AE2425D5A1F572848755D' +
    '8894222F37885C8D7D34C02FA95457C7EBCF1D56B3361DAAF75F43399CA1958609BF9654A0E50C6A' +
    'C0B405014AB252767A8C31F9D23BF9DAE2FC6A2548215B9E77F2FD28D049232763109367B1AB9A25' +
    'DBAE50ABDA2422A8F262A5A19D4035FDA91433072E5E1DC927425EA3D1DE222230889373EF0D08E4' +
    '75662B6BEBDF6C4E48AE442331D9C7FCBCA2C41901543AEFECB866D132090AA366A358939EF9AFA9' +
    '2B533A60444C95BB50EC36DC0478F4511948E34A4A2EE53B439EECA84B53B3470E86413C04822CB6' +
    '209FFB97C777DA0BE08F46F629DD64BE853901BCF32D1BB5EFC5512B0AC678154E98E862A9DF0882' +
    '43A1E279C55253AA3666AB343909B2421917FAF7FB4AB09A0268D3BE0CC9DEA933375975EC7E3AC2' +
    '2E50CEB3BE9FB3549EDF77D129FE75B1D24C2BECDBCD734F6A03349A4EBE17323985A9FFE33C1F68' +
    'E99F321B3EB918C0BDE1A53A79906BD5704351693AAF92804310DF2A4F05E1EAAFC241C308ECE274' +
    'B9B09935D5889EA1F58F580D2CADD2F9F9712279F0FA1F8AE4AEA4D993C55CE420DDFE5527FD0ABC' +
    'EE4FC8F547F6C335BF6BD6827CCA513567CF4744CAAC1C373B8F8E12E2234B0341F7F3974BD9A207' +
    '9EB5C48F25CF6400A1299E42F2D77804433E182F8EE02A1966846152FDAA949B9979260D1B511FAA' +
    '2A705A2282E3955B6B906A2FAA28FD054A9B78E424FA5895650E60A2B1BEB1AA6480EBCC0A01657A' +
    '8344AB896DFA7C023DEDA05EF642134A3AB9A783DA9AE6D189D2E2C2574ED909D757230885C01556' +
    '44F25166A0474F618FBB5066446C7091D6E6E7EFAA6890D3704F8FF5170F2E2F656FC7825C2F1175' +
    'A8DE14EC4CDC9F7D4D0996199D63C44E53D8ADB961335C954462B19B9BF52A67E3DE2BE220156E4D' +
    'C9445904FA9CF42C2DE922231FF1BE379830ABF6F00B46A79F470E2A4471730CB8C29A000EBEBE10' +
    'A2789A714F07F432962827AF53B321B8F7DE08A95D47FFC223DF9A44EE0FED3D5BCCCD948420E4E8' +
    'D452AF900D49C022A3FB6C0D6887C836393597E40D36613E77CCD8A306A21CF826D20D9F82351381' +
    '5D885EB74F04ADEDA4051BD0998F7C9A48015BF662D72F6A99AED192C283D936E65F48E9748ECF2E' +
    '0325518EF4F4B6991BC3956F3954535218B3727A721E31DAE286AB9947F13CD59E8F0FE7214DFB15' +
    '40731049216D8069DF7A2BE167623684D142F721444204DDBAA851629A43DD9F75554ECAC9BC397C' +
    '497B9097F85403DB3F6DD8AE83E4B0DE698F5E0AEF23FBCC21167E10EF554791F87DC9BD58F8EE82' +
    'AF76B0846510873159A534E129DAF7B567BC17D5EE78C9630EA506DBEDBA84E2D1FC5DF915490303' +
    '8A0FE8EF6C3AF6028D71CB2CF593596316A989A28DB6CF1814999DE9F597ABD1ABC20A168E2F2FC1' +
    '2F5C3D04D3581E0DF9CC2F890C207F97861790E0D1CDC67590DEEC5C6E9BA3EC2FB2AA3F0F047FE2' +
    '10CBE8EB03CC5F976A74DF9EC1086CD4D1A9B8E47888BD77095C305852E02C85F5B9BCDD6FF1C2B7' +
    '9E299EE6352A83FF2A32D5F71E8DCFD05167A9F6DE8BCE131602A83B998A45FC64AD3163D8B4673D' +
    'EFBE8EA91B0A4E181347D60BCB00DB9DD6A7D1E2553CB7DDCB62586D5D914E5C4DB82CEACB38834F' +
    '9B2681D81A5D6DCED5304496A84E5137982E6DA1B215701D50A77E22356BAC7734883E19BBD4A619' +
    'A8730C24C6C45B575B49747CDACE2F081545115C2A4B2AD772BDF51AE4A6F2E7152DD076FB2D8F4C' +
    '068A08720F4F6230578475FC0A5F0B86FA56D01C7E5D1C310A61D947DF93EC69FD96A5996A3B65F7';
  S_PK =
    'D712599A161ECD99EF5B7A04313D5507D612565F03AA9695ED7C2DF1CFA18056264432056C416013' +
    'F5AFE21ADB7F4A5CECA1EE694E580301B47EB634E18C25BC0C964470BEBF10AFDF7587BAE7FC40D4' +
    '5F94519A661A7CD04F7AA0D95AAEE797027D8CBFAEE682ADD859106CCA978F5311A3B53A1699691C' +
    '1E9E8BF9D2118B23BB1B33C5E9A02F968C800560A865FB826289FB34C936C825D1D0A7DB566B9F30' +
    'B8E42E7A9AE8F325E51C197CEA36DDB79DF537D1F4102DD44C01B59401770BCCF4D5E83291218639' +
    '29C91DFCA9C00F862ED7DBF3CF7558F6150B2B05D32F04C1E6E775350248DBFC0AEE58681B5B177C' +
    'A7BA17ED6C7B4BA7302DC0F349564DB0D83C7F7D1AFE502E27DC465704AA9B7AA50D8F5A14CCC570' +
    '1FAE3505D3AE11EAD8142AF9EE7991B3B216686126627C60E1769EC805000A6D94076905630D385B' +
    '7CDA7195B54522A81E8855F3D6DAC03E686CADF72A9142363770283915034ECDF9047F80A8F95942' +
    '73C7637B514BAF9ACE79A7B5D28F8B0EFD63A1F8B17301F499FD37E448F5E99F118FC7B2A4BFD7DA' +
    'DA350456CE5CC72C5646C57229ECD91082A85EFA259E2AAFF761F0A101A2269C8B88D94F7A8E4F87' +
    'E7B9F977396E71BCD2B2BF89AB8B829A345D37FE5240CE7216C64EFB01BA8A13700D4BFC9856EACE' +
    '22FB763BEC7D1E02F0F49B45F587BA6B3F571BE7AEBEA88557492C6C766D7AE298C7902F346DEDA4' +
    '94EEAC39C8767199252C9CE18825385596956F1F41B2BB534F761FA0C89917077B23F8056AA7405F' +
    '8AE4271B11DF284D4FF50AFCDBFDCBE880131B60E3534091A759531FBE6E14D6057BDB458449AE4B' +
    'CEC26580877331359E794E9D75BF454122264CCD13F9349C8250F0BB586B5BB482EB74524FD12F0F' +
    '61EDD28411D072FCEC556BBC5569AFCEF1A62C26078088A77ADA5976E6A2A99E98C898E5789DE786' +
    'B45708271957D88594DB7FBABE23DDBF1ABB12E715BD576483EAD259BA6553E95AE65AEDB5575153' +
    '2B98F32376BADDD2532E5801A255FA6DFC3FCCD5FC5110EE231450DDA25C4E09B89B577B8589C717' +
    '9B06D62ABA652BD6DAD5159E8D7B84A5879F075EBD31ECCE03D2F1E508FB229665B4232B90563817' +
    '6AFCF232D4C19AF38A812C8D2AF2776CD444E94D46816EE68E7B5CD5AC1622518682EEE88E0F6435' +
    '8181EF35A8EC71E00E70F1222F494615A4FDF7C769CADB0E13B2F15B5EDA38809A5960D25090C151' +
    '3636DE242BBD9A79F77CB45C81557D6F1D18991B37BECD4CAD9707B1F88920B69A55CD52CDAC2318' +
    '287D4B13BC9CBA6884F9A38054493BDF91B29C2B7A71E2DD8ECFCDD0259BFFF402251CA2C95042AF' +
    '917EBB669ACE5B354DC393A10F66847D546C47EDA695AD3A985834292E68A024AB6045ADF36DD2A5' +
    '8993A292B24837F61711CB59EA8DEB103F63F971E385CE0490A0709C890CAF57ED86991ACA729DBE' +
    'FEB023D4356B7144485B7AF45559C3FF722EC8FCD8BEFBAF41119A24C9B686A79C38356F3FC55720' +
    'F03AC89B3187CECC0C1247B9F8DDD82B8B415BAA6BC7A25E2529C41CC9765C768DC6A555D6114707' +
    '879E810BC4C7472A07E8B90B0F41B8B4CABD5326B190572B664127E2FB476D88DB32D42D50BCD7F6' +
    '37AB2B7D13E0A7D03DFA30B0B81F7D2917C1CA5260BF71230F7342A5BB9F9261D966CDEE85F768A9' +
    'B8B509F094C60E0C08BAC278827E84E6590141FF8C10E9B51FBF6E7724030EEB8B59731E47FA3D13' +
    '0407F833455386B66CBC453FF4BACC0467D747A1037EA57B3DE4421C3050DC6D33268CABE633C6E5' +
    '4C13D64973751798A99D8C53A2132E5BBBF8B2961B9E800F01940C700E47E4E1';
var
  M: TCnMLDSA;
  Priv: TCnMLDSAPrivateKey;
  Pub: TCnMLDSAPublicKey;
  SK, PK: TBytes;
  S, P: string;
begin
  M := nil;
  Priv := nil;
  Pub := nil;

  try
    M := TCnMLDSA.Create(cmdt44);
    Priv := TCnMLDSAPrivateKey.Create;
    Pub := TCnMLDSAPublicKey.Create;

    // 测生成
    M.GenerateKeys(Priv, Pub, 'AB611F971C44D1B755D289E0FCFEE70F0EB5D9FDFB1BC31CA894A75794235AF8');

    SK := M.SavePrivateKeyToBytes(Priv);
    PK := M.SavePublicKeyToBytes(Pub);

    S := BytesToHex(SK);
    P := BytesToHex(PK);

    Result := (S = S_SK) and (P = S_PK);
    if not Result then Exit;

    M.LoadPrivateKeyFromBytes(Priv, SK);
    M.LoadPublicKeyFromBytes(Pub, PK);

    // 测加载保存
    SK := M.SavePrivateKeyToBytes(Priv);
    PK := M.SavePublicKeyToBytes(Pub);

    S := BytesToHex(SK);
    P := BytesToHex(PK);

    Result := (S = S_SK) and (P = S_PK);
  finally
    Pub.Free;
    Priv.Free;
    M.Free;
  end;
end;

function TestMLDSA44SignVerify: Boolean;
const
  S_CTX: AnsiString = 'CnPack';
  S_MSG = 'E5AF86E7A081E7AE97E6B395E5BA93436E5061636BE5BC80E58F91E7BB84';
  S_PK =
    '60FC3030C9CC67C44C0EFD47C60D02296D0F73B25AE1B5F506EB9521D59A6A50AF94D2FB6C3F81D8' +
    'C4F6D0E737828747FD25BCAFBCC448E3146B9897FFFB3055D30604C55B633D1F67D49B11C103792D' +
    'FACC1195154F5C770D50329F52449B7C0D67881740A723BD3EF7B419B74F823F843A09C123DAB65C' +
    'EA4FFE95D2A14F991586232030A476D6B0948A7855DC551104BD97545BD68E5124013E31F9EB3604' +
    'AD2E2F137EF7826C62D6B0855D5C920DE6163C0A76AD5274CCD52FCC872E647C1E33D4E72B258020' +
    '88F8167C68A238A7113BD6CB425D30F5567488B9F798506334839FDA32424E091A6E22E3B011C48D' +
    'BA40421F88CBBD681F1AC5489CC3AC9F7E352B1A5EC7065FE3ABAD7B29894BDEA3CA06ECB229B3F3' +
    '52A8FCD69459C92C13260D31DA3347CFA97B8069B5F4006D9DC40876FD6D6B33C90A07827D77FD42' +
    'ECEE7802BBEB38925EDC8C24DCCF464F6AF5E84387732D61F02F9F24062E62A5C3176585CB6333D0' +
    '13A7B03073690FBF5FA5943CE65E106C664C82CD0338AD069672035FE9FDFA666F11B9C68B04BE2D' +
    '4ADB531BB81F6DDB817F5F5E52C0D61CC0ECC4F6468DC67185368511D87C614E331C83066BB89ABF' +
    '763F284C4BDFCC8697F12ACFD44AB613472ADD8F2510D5F664CC0EB6D9F8EA7D1106D3346938C70E' +
    '5513F8041F5543C749C6C3966513ED346780568F99E232E75B7E9C3489B5B1022AF85DFB44D4957C' +
    'C436A181F22A0AE03FF222DE0C54C727EACB9F590C78E21393EAA12E2FFAB6A0EE346CCF9B236DB9' +
    'E7110305A150CB2B82A639F3E3F8956E0BF29691DB7C11BA94D7483E000B78D039DB58A873108BBA' +
    '46A981E5F4E3B4915D4BA10391C30D8CACE5DC11764C3F47B14475085B90E91B00C79D37FBA31A1E' +
    '1E1D9B8300D2C30F52EBC8FB050ACAC3DB7A14A05ABB86FAFCC9164915DC7F0E790AE8447ACE8A9D' +
    '380E5905A914D0135612A69AA8F108F3220103F072F362B9243B216F76E1E0ABF9C9A054C00404D5' +
    '9F8F804A07484C7FA8EF71588DE20B7F0469BAB32EA0BE3C72A1607C6051F4B0B40BE9A1CC28B86B' +
    '087319EC1C8223B983DA3C201F3953004E51DF3096D87517B03EC831DAFBB84345A2EED2AAB12FFF' +
    '190384EC74420099978557AAE0A3DF5554BF8DB4CF3F3FCB4C337E12F9F756E9CFE681DF345ECF90' +
    'DFF5301A80ED8869A5BB70E533576A3C8D48BB9F75718ED10ABC6E18654FFC44380AB4CEBBE02BF7' +
    'EE6654BAD63AC5A3C580B68E5859137AD8BFB5C57A040C23F67864DBA78A77F5A17DB4A7BCC175CB' +
    'C624E7E6CFA497ECE4914C9159E11B067229AE8A38EA468FC043E229D343D143CB2BB38A51CFE492' +
    'C2D506D9B57CDDB347A7FE0FE95225456FC5FEB799B50C895737A6BEB581AD0404CDB9D8FB77AE8C' +
    '39C1D9364018576A750E254B35462D34D4647AE5E608DE6C899A8A38A462AFAD6317A52666382C8F' +
    '7770E3A945B14A9763A7859E564974B442AE6CE5C22BCF5CEB78A4B79DD94DE17D0C31AC58905B68' +
    '50D884281EFFC86A1D64FC72540BF92BA88B1FFB78B19514505DA0ED886F72738AAEAE5F7740E8D7' +
    'F760A95F22161BF66FF64CF21F9DB33E048807EC3B988D2CB5DA7E4A974636CC44F97129379B457B' +
    '976EDEDEF82338219E16F66425F690371AEE0C7330FE041EAAEA1F2F58C205178115E68DC1F7B1DD' +
    'D2E884C3A8772D29E6A70AEE30241D69DFBE4D64C00C31FFB7DB9B618F412AAD7662AB8124FCCDD1' +
    'AAB273683AC778B992BED45A8A34F4EF08FF14749B26A57CA14F1771D1C5038217EA7909050706C0' +
    'A084DE280CD734D90EEC8B56A579FA720C72CA98F778C0369F501FCEDDA11277';
  S_SK =
    '60FC3030C9CC67C44C0EFD47C60D02296D0F73B25AE1B5F506EB9521D59A6A500717CB32F9BD049A' +
    '77B2C5511B777B7422B6FE1EDE80985951F3F22BDCA5F6AD720842C17F42017DAB38E431FAA7878E' +
    '936C1F23FA61CF1EB7CCDCBDF816F8628C60983C78A70D512BA37EA469CD9E7D0C53F6F0B9196614' +
    '223FB31B429A6A3B03232C4C00111CC48543468A13090AE0988104316DCA34248090812301201425' +
    '229B402E92B6911481259846860380248208520AB929A32245111586020648A398401A190E90C829' +
    '1341111AC36514806484484D5A8031881050DA1412CB180A22C82DD3C8404A828094C44992288E58' +
    '984919038062C4081CC810102549D320620904451B03261491609116485CA64C51422ACA10861121' +
    '821B210A53B810490451D4147280802C0A252E214065A12806C3927001410002416E80A088844690' +
    '0B45929C84695C300511076DD0328D010321090026C198444214720A01281B836941A6311042729B' +
    '120824852D8A144108C31008A5850CB510023351D130241A811014902D40960DCC82204318851949' +
    '52D0A6054A384111260EE2285024472063820822A72D4C324159460D4AC42D90224614B164192052' +
    '92040611C3650098091B460E4BC6301A014C6020081137044444706424001A3602D9A4251093810C' +
    'B930E0022900868C0CC3451428425B302C03B98581C6296346491C1600CBB071C1228AE4A4915144' +
    '8A934291C9002D1240118B22011B2044020044D0B6205008065A90455B042209C000828851548248' +
    '5B0081DC9001C8964C181050DA247120223052A04461960C528025D8B888CAC841D8086D12314D49' +
    'A6800BC150800685A48851123442410832D802620028845A8269613462E1B64823490C5C26411C08' +
    '90104662A2048902A70903154E00B500D1048C5AC26C1B456C89300A24C505912028C8B000C19648' +
    '1A396C80B849820069DB406850420502440CA1844119400550068901176A09A724A4C21060149003' +
    '083100450ED8422E102430522066D2946D40124113C164403064A09620184905D2122A1A476ECB16' +
    '4EE30841E3A07118B00419A4809A826109B96842206A41186400148E802405C480888CB40940A860' +
    '5840458C0212DCC824D4B82912C985404220E4C4302082118B882853C824CA06050C072DC8A22C09' +
    '38008C2049E08050530022CA94491BA40911984923179152042903456E0AA8241A322CD0144C10B6' +
    '308C0222201172DAB820CA84811492448A3BA3C98A607F8DADC136AB4891DB9E86BD5E76773B777C' +
    '3114BAB40C5CACA95E840C73BB3E7B1FE16E8512DFB5A72491CA15FDE8BFBFB1F72A73EB11D3FD6C' +
    '887D3F9E7122CED067F0AFC4527233B11318527FB236C3704FA5FC16301BB750B6DD79F84865CCD1' +
    'EF8F1180510D4B56947E3D993AD9200093610E5814F926906C84F9D22787503A5886C78952DDECFD' +
    '3D7893258C44A59D899420F12A9F405B897A935F423D7041778340A4F41F07935129F4E8B1505192' +
    '4148EAB0548A880FBA223F75031E33706F04896A600EA02BFC12D97AC3BCD626B3DED6F3B2BA46F4' +
    '74DFC1266C4C1C915763C1BB4D71D3374C01CC5E09407F8C77F575BC39B258718C4D85CA42C92A6B' +
    '3DEE6C7D52D0985B1EECD83AC67C42B2A71710193D95C84EAE2D2CD5DC9A3C9FD3ED22140D8AEE53' +
    'C69DB30653AF6D14301C8E8721AE002AA67F8CEC7C9CDA6050B4CFDDE3D5D15EB8B28711F4B38762' +
    'A6FB688BA952299AC83374EB6D03553F243EB3A537C5DEB2C099CC3B5EDF798031D9DD131F956301' +
    '5FC4D2ED25FE74907AB61E6163795D45A465E9CACDB9ED896750F5A03A77E85FFAB280F22525BC8D' +
    'C9A29175650D26517938EC3A4707AEDDAAF6C67CDD499B16927EE3B4B435E5FFC29ADBD6FB4F7E73' +
    'B62058BB5EED6EDF9F7FEDF1495193A6A76C8D475EF5B6605915B07F40E7C10EDD60AFD71384809C' +
    'BBF94577C6D5E51F8395BB2277A886188680892801B5D718FC4DF915D3778872E44C42BA7E0D6E2D' +
    '7628105C9630976C50DCD9FC0239F2256F06656E9F44BF8C993AB9EDDDFE61F0C0BD4CC7E7FF3EA3' +
    'EF0863EBF22B5B5FC0696000BA2A1874C8CDF4D901D831537158BF0E747FC365E83F490640EEB309' +
    'C1079CACB4D9B4D7FA46321CAFB2D13D101E988D52847854C09934659B62FA1F7230DC94161060B2' +
    '42C07C1DA0B1F1AC9FBAA2BA6276729DD0660C057CBB1544AFF5FBA74B0EA7AF375BD67BC469727B' +
    'A9D35BDC2233E87E405EDE5C97B1204BFBADCBEE037EE6C6C550243248522B2AB1F195059C0E81AB' +
    'D02AB579C7BC9400691D715288B22FABA998CC02F7640B32F9836069A888C13A9676C8E1B252779D' +
    '0B4DE6E4F2D68CBDCCBC15E5345188094756B1CCD4CE0D10F11EE89E3D7377F5C96445BE8676D9FE' +
    '7CDE9CE497D128F1BA42F40C38C1E0931497BA8B28EAFEA41A5CE203D19E1F44BE11DBB0B2876051' +
    '72690F0988B7427F9083B9D583FAEE3487F6B75F2CED99DE2E67F8D9987EEEF91E51828FBC312843' +
    '2BAD5BD9B3FF385858B65CCF1C79905BEB655E46DD9210F87C7D43FDE100356984DBD55956632665' +
    '864380EDE7E1CF4450DD24A463BE1BA934878C6872F60E4AB96ACC44B157A728F0EC32FC6A7F0F1C' +
    '4348911BB715AC36F88D68555CBCED2B4B1EDC339DC6FD36BCAF72FFD808038146249964FF8C5F18' +
    '23C32A0EC44F3F97192573BC79D7DF1A473E8D945D8F4D72465307A01FB6B165729CF56FACB03F11' +
    '062B1A6CBD9B300BD0F4FB6A54CE360F52C378E6E150D8BBE3ECB7DEA65457CD3121914283481D38' +
    '0DE6B527744EAE2BF45FB09ED5FDB7C0F7CD45427540E674306E7B4303CF0825588F606EDD795C31' +
    '211259860F2913E05061E9E8D78B15DA97A5492532B4EDEBF63C901F620AAF9ECC9F2BE5B5E2F028' +
    '64ADF1B6A2663E9D654F20ED0FE14BDA106336AC76682E84D66ADC6826176240C3BA7A8D0895ADD5' +
    '5706069B378304FB6A45AD9A0354C4D0DFA13A2F97244AF6C1F03B91A6DF0DCD938BC2DEBC275ADF' +
    '1B9D706672C02E12B1D2FB129BA4D58FF2F869AB929071E5BC4BC420E2B346F56424FABA5C80E141' +
    '8CDE6542F74297D62F34D0E84FC8C6297BD95B1B5AB5AEB3113B6B3A27BB9866656BE9572A864A14' +
    'B2DF2185AFEF09C70EBA43046E4536CF7938BA7E45BED3A580D5AE98074F254D3E1DDB15EB705D8A' +
    '3482316BAF5A02038D4186EA9FF40AD35D3CA955F5720932DBEF69016773C5AF8BE1D7278BFBC58D' +
    '4D500FCB6AD9D7ACC9C6EB0F33CDD0D0F49D40505D9020C0332756D35CC5AEB9C873447C90C25B22' +
    '0A3FC1888557692B9F33D1A113C56DBBFB018691B96961B73826F58DB0C411F548395DD3F5BDBE77' +
    '7BBA14152C6910F07E2577B08247645D4BEDF0A13A87F7855CBA4DF7D8687AC0DD1241EBF3B3EA93' +
    '98E7CFD4CCF1A237AA03D840030206FA7DE3FB35A4CAB4A2A38288F710A16FB0E24B95851A18F11A' +
    '1C65DECA9A66B9C3B9BF1863436E494ECC82E5A04209F00FB03E0B6B094013ABA55EB3FA9B325521' +
    'C50DB1844479006EF65B088ED1AD9CA62F4A8A03D3D3EE041D3C44D9B2D15216CF8D0DCE2A92C6BF';
  S_CTX_T =
    '9B13BF381D9DC5DB51F7DF2520E41BDBD60CDE6C2F175FD92837226843D04FB25D2084BD3DDFF2C5' +
    '3AF872D7E3A1CC863973F060B3A4391FD0FC180F6300D4C3CEC2C6ED6B554EC66D921168372FD641' +
    '47856818931D74240DEF128BA65F584B60A906BAE2130CA28B847E28C8460E';
  S_MSG_T =
    '968DCBC54EB2CF5CD36F04BB004EB1700E24915CE0885BDA2C5818C1A7E69414AA1968943A5C7A6A' +
    'E152A2F130A246067162B304590607FF210621C81CB0AE7661345D465522D2C229EA62E8887DE14E' +
    '213471565CF6490860BDB70D1EA355433A2A6E11F0683BE3B80FFBA20E67BDD914406C71F4E9A0D0' +
    '464EAF91068BD2248D71504556F58E8EA6663FFCC78F570F4EBE98F0C8E2B48A6FC5077FF7E249F8' +
    '0CC23265760C97AD303454A97310964DCEA6735083DEE6BFD6C651EEA2BFC3FB0FB30C5DDD25B5C2' +
    '0E1CCEAEA28B51C1B4AA3EBADD59AB191F108A2CCFA6C9FFE3F635C5D24AF80C6051A92FF706CF61' +
    'F321946CBFA6F24478DEA7D5311614B84CC6B472ED44E24816FD60D09E24F7C7E6BC42E35E16C697' +
    '68220AB4920E0AC1214754EFCDB61E3604F282853E5A6783933AE43CD736FFA0B582280C7BE73A99' +
    '839E2A05A78B02CBF91A195EB67B1A912F859CCDA88D5F4C4DFC7AD08118501E50390158EB960830' +
    '088DCA36824D9D3052A925CB058BD384F6169CB0BB0B58C3509CAB54AC6C3A5450A2597B115663E9' +
    'AC36DBDADCA1BAFB8DADC426EB59D26209DB8DE6364A672D83016CC0876131BB3D87098E6D629FDE' +
    '82E29E1399BE55BD62E1DC4312A170B6C4483C7B7F3DDE2CC15EE540163543E61F9F87A7002B75C0' +
    '7CECFD658DF31A23C720C3B048BF7E0A899F3F0F7ED2A9604669362CC438D787F8853BFD4767CB00' +
    '6E6412C095895056CC4BE0D7B36E45B7E945CD58F38887DAE0583A99154A556A76F27DC48A92A613' +
    'C22AB54B13B7E0321E51489DD142F3EB289D523B9D12808297F48F94A0630831E39F9AE674B07733' +
    '94D7AB96CDED5E06871E2893C489B35C102AFB8BEA750AC9E7158A30487A0A503017C5506E91AD92' +
    '7F7909AB2DF0476ECFE2607E00A84A59281A277A80DD8090C955830BF7823E71A9FC3335CF33E30F' +
    '8F0A0DC80BD3A6B323BDD3AD02568FB2AFD8DE11FFD779DDAA6B52A65C6D17FCC72AD10F902F69A7' +
    '6683E2705FE9DDFCA38FA3E62FAA39611A9EF7747A0C37052E3296DF52324252BE0DBC3838937BA2' +
    'E0CCB39373452F5D092442D28228B422BB1A04DA8BBDC85722162B5E0843855C707A9C2E2FF22032' +
    '0D219D1B896469EA6BC47BBDEFEB1C7DFBE28594773C7EA740CC37EC121FE737D8F4C6A83AC439A8' +
    'ABA29A87EAC528A866AF4E162AF926D4C9D8E0C30D158D064B65821894832C4AF9B08D9E10487F20' +
    '8F45019F25329D5B0C5B2B670F32583841ABD069E6B8ED674F7448C754332FF53FEFF6664DCEB11B' +
    'CF69D082EB7CBE4177E1D1403EBE1086AB4E6ED74E6CC4DDDD17E3CD74D01A13';
  S_SIG =
    '0476B8DA70153726E115877BC45607CE80DE0B02C499D254CA45E095D627928E0E9C38A9BBDCF4D9' +
    'FBFB1869BB4D4003CB70C46F5B225A6CDEEFCDF96611941A676FA0AFB6C6F9B39C0083948FC90D99' +
    'BDA6FADD9F508D213BD7D0BAFCD399E20927E7E334B006A52139865C18727EE0CF726578CDF82EFF' +
    'C8659D2CC6E68EEF3EA037F9F6B411BD88BAE3FA4C28B3DCE901F73F681F9E5F106C843F20DB42FB' +
    '7DC9D9A4CD7AD5BE7C04425FD5EEB53B98241EEFA30A63455B10C84591C3AAB938C4B5EFB8A12D09' +
    '805EA5710DA1E20DBF0670F57997482F95CBECAC3C4AE9A6E2602CD46950D2F4339C7CDDE3085F3D' +
    '38BB0C43B2C5B767C1DEEC2A6E3199730EEDCC6B2DF10C036C2E7AB399D893B3675F236085183E4E' +
    '3A859533FA7BFF97DBD12710ABE4C813479AEC23AF042F6C6ACCB0592D73E2A56CB8A61F1977F29A' +
    '93F05B592D7E7AEE8911B2D93645C3A43E6AED6F6FFD84DC49E5DE468C480DDB493E9C8E3DD92E18' +
    '8E39E637F93377F4F0AA2FD6D30BD1DF4DFF3AA02849D20509C3F6C6F23085A792FEA8D4F3ED254E' +
    'BEB10CF1A2C34206ABF508866CEC9953FBFDA7B9B6BDEB08B5309F68B757BE8ACC22D22E1976DE4C' +
    '8A97FA6200DAB292E7935C7CC6900B3F1ED8B8990F9F2F18FB23F5FD2ACDED0994D681AE2416D263' +
    '5159622EBE009137C9C8ACADD41563E850527EF4DD5D0E12866732B13EECED11FB8AC3CB4B657A19' +
    '80EB8C0EE3A9FA92BA5C82618EC8CF5B2D6514B1C3386A27A6DC0F2D9AA7AFDD4D93296C772D0C91' +
    '91E26B02DB137DF2ABA473E93C04001EEFF610BE56FCC8DB9E4D717BF2F1ECC3FF9A663F95331882' +
    'F7176590AFDA22BC697D23CDBE1FC6DC960930220CA063ADBEDCCEA4D6388E4BF05FE343F29F5526' +
    'AE7DD35252DD2BB67AE0FF9A509D85075A7B8A073DD5D6A73707AC22AC7488A03F04DC6F3334B808' +
    'BAEE98CA1993CB4BBC21E7CFC5406B2730E6BB31D1E2D2134564C098DAFD158B140670472440D340' +
    '8948CE3457B2343CF9359B3949BDF148FDB3BC4915063FCE99DDD9467756C8F883D83C8CB3575506' +
    '8B1F9ED99DBBD52842D277CAA5F55E88F4DA8763B4C1D733FD0F81999D858B98BDC2EE8DF9E8F46F' +
    '7A0C73915AE1C8FABF707335457FC61937993DC99877AB4DF122A8723DAF4B88B1DA92E398FC7FED' +
    '2084ABF5AD370538001239BF46058DE89A7B90717627A0EAE7C62EA1832EC065A694849AD992B0C2' +
    '7B310A55D6E950F6BC470E5818FC0194F0B1552A9211D9A8EF5C940D1719AC315EA925ECCD107674' +
    '1A2A1A3333E0021FB5B16F65F58181BA64904F6550945FCBB855D6AEF12BC1E3C9DAC6D6E3E5F3B5' +
    'CAF2D0F27DAFAA97C1426BC3A740B17653DC809F2C0CFBD6409E6995FE905198CDBFE8987B45E683' +
    '9B72365FF16E465DCF5E139F02A9538722DAC7E0DD1CBDD462442FD1200928CE291F651617D40597' +
    '8A8A946FE37821C9563D2DD24ACC282CC9FCF7736E228436586F352D856B9009BDC33A834A626B58' +
    '0F8A91003A12C2D30E5B7D49883EA3594DA819CEE2D39D8C3CF691E42E6554B4562FEAA300BE3950' +
    'C8264E3F431DA17381B11936972C165021285608412836085194AC26A5C5868DD2BB9BFD7B561E74' +
    '54AB63DAC73C5B1B727B8E1DEA479E47ABA6BA3FE5FAB099CD3E856D0654F01E72FB65C5E85EA48E' +
    '92B824F550AA0B9329007FF6543C0A5160AFD2C37F3D7975972127610466303E5B8E73CE4E973875' +
    'DD68411B2DBAA0CB235903BD5C95ABF58562564E37B76BE22CC205EA48652720F1B82C433762C30D' +
    '6115A6A958C5207B0F2F4D1B6FE34E487D88E8C815BFFF1C43442284F346525863198F4B7B3E0B6E' +
    '3326829FD1357DED656DEBDE0BD590E737337C839C2C956A863E614FD6B4AA68FEC99F2F34BCCE58' +
    '5FB152F7A56FA96FAD64544097E7716E3EB376141A43E577F8A2E886DB868B19521054E340EA2D65' +
    'D63F9CAB102D68C37962ED1460F3A4FAB561210F742424F7E63BBE06C3D0501BB8EF30708376142E' +
    'A5FDE75D0DA78E757C865AC24EB5F0A863DB8D681CE2AA1A645756687F323046E5DB2FDF9363EA80' +
    '9133711A4509FAFEDD4A8020EAFD0214677132B1C604927BFF5FD5BDB57AB8F04D0097476C4B1CB5' +
    '5B2ED0B19607BD95A7F71BD59D9370E4ABFA72AA383ACDB30B9668388F6F9244325E9A6D6F772C54' +
    '7A8D9B36A486F6832C547B2729CE6564B532B232170FB2B452E3C7CEC81BD37D0B5DD44FA05EBE35' +
    'F03C5EF46143AD2DEBB8EA0E856CDBDF43C97398819F7E49303632D96AC7F8CE3BE4217307CA2850' +
    '4EAF1DF7F2A162240B3DE7D85BABF5B6CFE607DAA439E9773D596A81C60624AE1460A9E41A2843F8' +
    'BDBC5B7A181A4C59982CC2CB58BA5C9447A4B6E58C75A0B41A6D2A1365F6B44E9271AF423A3888F3' +
    'A1613DF6E84A7EF05BE50D018A517BB7B312A04D18F6980B281A582B2D78F3FBD89205862171DDA8' +
    '98EA906A0D6B08D33E9B843E75F593E524796E321B4FE6F3659FB001A895D7039B62D54C3A999FEA' +
    '798A4245F29F6E992DE1B0CB882C1C5ADFE5FDAFB57CC6EE52371A76F65F632C6D0A76992749268F' +
    '0A59B4951AC1AF7D1CBE3E484161B5940BE065952CCEF341D88FC25AA906FF11C64595CC56EDB029' +
    '3948EE1898041F26FFB9BB03ACD71207156EBC072B1E0F54E7E68E153FA8036FAAE2BB0D1904552A' +
    '96CE167C00E2A5123F76FB762B4A2E66E81C35CD667A442B36A09C2A29812E3D7E99D8501E425969' +
    '0FAFA138AEF08902E3AEE8F57CDCB8F39B20894149EB8E72F919880EFA746C466C01D64A04F1F337' +
    '9C8EB95A02C5E5180104BF6222FBDF8110AC0EC316F89DC224FFA6973F37DA97DB05D94E503D08DD' +
    '318C30186BDA2686F46F5B1851CE792217F63E03C6EAD5241365E41A8854E633A43C2671C3650F62' +
    '874ED76395FC0C627830290BECED03447FA999FB93BD75B0DECD963E3076AAB489D041B44E9F4F4C' +
    '10AD80FC34B7F3FB89665E3989737750D1E5317B07765D00B75185C253C26DFA8FACB0040B656F44' +
    '9F35B3E84A4949AFF414F3F559357A913C0168E8E6D7930A2E9C6B5C9A7803270D32DC85EF4DEE3B' +
    'D7E90DFCC51690138B752A9BC2A8CB37A9BE25DCFA16C409519D53FB571CAC6BDB0A1969B6A2F9FA' +
    '951DAE001401EF3072D3C2BAAB1DAA6487275D5150639652DAF7CFBDAF62F6E4F5BAA4A239094847' +
    '138102DCEF4F9CE84AEF72AC7EBD6E8D24E8A5D3327FB8F343F14CE0D9CB66CF295E13483C7034EC' +
    '8A7C7439A9918CC1EB1ADD16C416E326172B3E6A6F7785888B989A9DA7ADAEB4C0D2FE323B54565F' +
    '88A4D10B1220283335555C7D83A5B2B4BDBEF2181D3F434D5055577E868C9DB8CBE5000000000000' +
    '00000000000000000000000000000000131B2B3A';
var
  M: TCnMLDSA;
  Priv: TCnMLDSAPrivateKey;
  Pub: TCnMLDSAPublicKey;
  SK, PK, Sig, Msg: TBytes;
  Ctx: AnsiString;
  S: string;
begin
  M := nil;
  Priv := nil;
  Pub := nil;

  try
    M := TCnMLDSA.Create(cmdt44);
    Priv := TCnMLDSAPrivateKey.Create;
    Pub := TCnMLDSAPublicKey.Create;

    SK := HexToBytes(S_SK);
    PK := HexToBytes(S_PK);
    Msg := HexToBytes(S_MSG_T);
    Ctx := HexToAnsiStr(S_CTX_T);

    M.LoadPrivateKeyFromBytes(Priv, SK);
    M.LoadPublicKeyFromBytes(Pub, PK);

    Sig := M.SignBytes(Priv, Msg, Ctx);
    S := BytesToHex(Sig);
    Result := S = S_SIG;
    if not Result then Exit;

    // 随机生成反复测
    M.GenerateKeys(Priv, Pub, '821229E3226BB7A7774BFC3C3593DA9C4DE76255374CC4B13F9F62860E0C3E07');

    SK := M.SavePrivateKeyToBytes(Priv);
    PK := M.SavePublicKeyToBytes(Pub);

    Msg := HexToBytes(S_MSG);
    Sig := M.SignBytes(Priv, Msg, S_CTX, cmhtNone);

    M.LoadPrivateKeyFromBytes(Priv, SK);
    M.LoadPublicKeyFromBytes(Pub, PK);

    Result := M.VerifyBytes(Pub, Msg, Sig, S_CTX, cmhtNone);
    if not Result then Exit;

    Sig := M.SignBytes(Priv, Msg, S_CTX, cmhtSHA256);
    Result := M.VerifyBytes(Pub, Msg, Sig, S_CTX, cmhtSHA256);
    if not Result then Exit;

    Sig := M.SignBytes(Priv, Msg, S_CTX, cmhtSHA512);
    Result := M.VerifyBytes(Pub, Msg, Sig, S_CTX, cmhtSHA512);
    if not Result then Exit;

    Sig := M.SignBytes(Priv, Msg, S_CTX, cmhtSHAKE128);
    Result := M.VerifyBytes(Pub, Msg, Sig, S_CTX, cmhtSHAKE128);
    if not Result then Exit;

    Sig := M.SignBytes(Priv, Msg, S_CTX, cmhtSM3);
    Result := M.VerifyBytes(Pub, Msg, Sig, S_CTX, cmhtSM3);
  finally
    Pub.Free;
    Priv.Free;
    M.Free;
  end;
end;

function TestMLDSA65KeyGen: Boolean;
const
  S_SK =
    '43AD6560D3BB684667A559EE6EC7C816020E5B65671F270F2353A8C912B6C26B3DAC8325D5208FBB' +
    'A385BE4D9C77611961D796200072F900727D715515AFE3A3839104C526A5806257D54D22CA1035F1' +
    '7DCA06D2CECBCFAB310C994F7D58E62CD8B9A5CAE7FEF2775FB612246459A94DEB8BE511BAD4C007' +
    'C5607FE1C9CDB7C91871042734805760071827016058751083551404015714520370521746384075' +
    '61232566011558506027142575452430021032256102148680101016217778047720268341572613' +
    '78285714503441714757160073732356454870013823556823652414027414467352117628737408' +
    '22880881412810353573148554078386048742028384216832477015164257364347170070476826' +
    '42523476171210235348518663656457444724377120164183683438543210752815327213436862' +
    '37227178234275545600726724632063482165470350156412367821112008871272133015375501' +
    '24661416832623134008275127301110438274170284760336777540767006607015458542733644' +
    '77527653031481033526727053663502446271203330764885318152610381162634273374774115' +
    '27372436312303640105630174273246816510212641412477524607721300642371627800401363' +
    '55137636372534687431201613276233550463451322058364785885052601380733632615061100' +
    '46525885585486470783338728527025371757228367720826786632442648888664567641215343' +
    '44424088315124321437108104736487517350118228336624244787454758840387161255671842' +
    '08768860650416622351137275746336756553777453220673618100250174607842037110877466' +
    '57367767870635317826728708677118636774557228730464268435746880724165042145653361' +
    '34206816440212168446314053402810218615206277587064842382778073506378438575728063' +
    '63145528876076540884216485115253516510151067614034047278055348333071554060501056' +
    '77751254475831305621337501545847223755453130146855043504633775825214046505831782' +
    '68185780510230484347714884333033718604346122076820516810072742355046510511050364' +
    '85727048777861140347683323582177270861101745674724017721068503801314751007050330' +
    '68137545145731165715511552416705816565437664874846745602571867462001877773648817' +
    '20453658314312756473640018266428267342685700018774244018117438545651832682404883' +
    '47571103622607068764060037426823556464876211866704285442566432428611512475760138' +
    '18486858637104702643412736346843582065646835415016716855362122176312878427117031' +
    '46253503861323702635710641403168165420138152652710726756872661205666788778252318' +
    '80065251181231633822285122458443015318633364000784236458252384030804427720857853' +
    '02804880023810815632470002070583446286620075101543532564262255420231471047745076' +
    '82666885626813008117821757115307444846404057433872085553236700784108754776448157' +
    '40410477021318650866587571047373875644673507143062246681336181716752823720657328' +
    '62882748028156275776088240167187265530483683611254358755481744280472180622102828' +
    '84001702540737560173176405776471080717638445652451150365406628342207516552873422' +
    '21621752356237133878057213237585028728713738001774675108141608126487445165350021' +
    '32422305484115354484258445332215383756671144327484627347377021066832526228313864' +
    '06021017072061511441845036768158021388323027514722271175615420362623266154313381' +
    '65116758685720162285157116377643231431243852733776584261054544005228457187605483' +
    '01842253570007421821308315311248062732561802128461130725500206157265486367318662' +
    '1261024440273457334313817453457750444B7169D30417F278FC99324BEDA10F1E123855584D14' +
    'CB6C067B8E8D699C0F991FBD7BB1791F7B30DC418483BB316CF9613E5BD127EA122632DB2F204D78' +
    'DF5278020117CDA39DD5A0EFAF087C794387A7EF66B8420B966B2326A802B34A3E92F71BED842371' +
    'FDAE6B0D61D5C72DAA85AF99FB4C97B14E2E4BC4BB11D3B3434ADA822CD786F43DA8026FF857FA82' +
    'D2C32715DE8DF951855EAA8644B2BDE0CD5E8F8B40624DE06C6BA60C4E50308A757C1B7B24B635DE' +
    '0CFE2EFA64F14AEE1DB19A8A834C2C5B9C45CFE2D1842DB27B471E70D176735E05D4279270CB78D4' +
    '60834CDA4A25E344E5C5E1A03A224C3857E4912F944AE618B1F8387E8B2475C9A20D59694060BDCF' +
    '3D83ECEA1A1445EE1A33F9C2ED52C39642BCD39C1D4B09062D121D028B220491A6B3C66B81442C5A' +
    'C20B1BD3A047F1E57330C0FEAC6FE5CF725FC76DD3A84EC4FA9C9164F2061494AA89B0367E4A24B0' +
    '8FE656397E0924EC373898F2AA25FC62A9E271A0DA015D4E4FF2189654594D574B8F8884DA9991A6' +
    'F54A2F13DF15F8B0F67D7491207539488505D591964B4A1F22650E881E74C84255E81D016F5A99AE' +
    '559686098997CCD1AD102BE86AE4A1CCED1EEB057CF89DC103CC4026E4B244640055AE44B08A85C3' +
    'C3A0617998A17A533ECA6904677D351E1C10575032FE493577AAFBDF32EE4C8784A1DAEA2F04EDE5' +
    'E6ED69F6CBE75D1830EBF74A3F93C13A199895A4C5528BBA9A3DDC3BE87EAD4E2FD9AA951A9F5749' +
    '502CF89FCB57F47E95B479372BFCD80D0203C9680D94B43671C12B35C511A6CCC24A284B6563C6F0' +
    '833FA5E82261831C4E6D3740135200CF6D10342A808AE47EB0CFBBE2E9912B7EC2E53E69C3873E21' +
    'D52B3993E080B7926E95E167EA3EB9D3E52B802B03BBE35CAB65C938ACA9DC7C282A0FE2E0EAFC9E' +
    '27A5945B2EF7279145DD5A4F4DD63C30CC9728CB0849D5BD35D43D604B42EC4D76C0120A38FF97A4' +
    '95C38D60F573FFFDC704367618B919791A6806A2B9DA834C668097911568C554A821307E5CC522B6' +
    'E607193C1CB7952E221957F2684B4700B5AFBFCAD7852735AE040D63ECF483FA2ED1BC358A274E80' +
    '695AD706B47BDF7B8BEC73DD99187B8DB51E6CF0D04BCE594BB7EAB045C628B45C195005BFFF6F43' +
    'FC61C2D617FFFA5C06B195B7284F15D009800856149930309E693060381F6958CDDE8EABE65B87FF' +
    'FC6623D239EA72278E69AD8C505B16073C8A035F2BB9458FA38A6EC27B7D30E9F40B82F657B21B4C' +
    'AE98FCC7E366EB20199BEE79BDDEDB3629A43FDB2ABF59CD2E37CCF15C59234DF85B15A1C6440F3F' +
    '66DAF010E6BB7A3A0C1DE62CBBA224BCEF2C9233702B92B124A29FE54E45CA2C7BC371D52D9A9E11' +
    'AD0C84771A0181378AC896F17398E6583B84A3E495E0AEDA09875EF4B676AB813C8C9DDE120203BF' +
    'DDFD9B6412F5A10A49109921185D4DA5D89E455B036654F6108FDF36701A36538EA04AF2855C8D2A' +
    '6E7810A45BCD4F8940320C8C1CD179070307906E016815E9F37D3A32943E164DEADB8C1A393EDEE5' +
    'ACEAC9C7153B55805FEF3A5EE27234EF040B8F269FB96D3659044C8CAC05659F3E9F603C3C165EA9' +
    '749AA1C0A7BE268E747B57B01521CFBDCC63B3AFDC45CF5EC82431C6CF18A98357B1E1FE184818E8' +
    '896674E867314626AA5A50B3333CFC3B1FA1E9B8BCB45C181A6EEA07FC14566695171D3EAF5A60E9' +
    '2A2ED2FBC13E34C699AC1CA57AD278471601892E9674E2763ED697F80EEA52B06A747AA4B0FD2DEB' +
    '55D565CC13C7AEBC2805BA2777E19D884A0A618B08D6BBB4AFFBE8A1C1CF8DDE4258B11E50904F48' +
    '8EF7A73FD0E0C141F493BCD0C648F23DD4B721ED5CF1041EFD1AF46446418EFBEF514AF9714F7127' +
    'E98B6EBF17755ED6C0EAEACD61400E07F93E885748E248578784B42660AF8936CD565AFF9DC16647' +
    'C014000039E3107B978FF28B3CA0E6C8D552925D9B3B7F9190363089C4BD709CE8DD0AE6AF6DECD6' +
    'D5975539EBFC011975D268654E0A7BBE49AAB559B01D9F64CA7E858BC8F78AFBF9E7B360F0A039A3' +
    '3ADA0AD70677C6C90AACCBAEB5DD384CE28126B2ED11306662819BC51E71E1EF93CC989244FCC0DF' +
    '266D90FF57F09FB4EA2ADE80AF9C9A1A67437F6FCCCBBC9181F4BEDC7B456AD592AE4ADA35AEB613' +
    'B50D46EB3DEEC8F9A5F36F1C92E900DFE55B212EEE6FC963E0C149B9AF83E256EA3021AF7BBA9AFF' +
    '46E2617BEF211987F1C0BB6F63737BDE142C96C6D284421A3E49B141D3125580391AC5EEAAF7D3FF' +
    '1A61CC15AAD4F64F712D0A26F57A869E28BBD44A056000819D9F8A42FAF0046517B3B8DC43F04B2F' +
    '438F147462ABAE1CBBBCDE086CA358C3596C34BD237509625AC7033CDC40EF5EDBC43E0ACB41CDFB' +
    '813761200837E47F9780F5FD5ADBBF03065DE78FCE30DB1C0FDA6C64BC13C98CDBBD518A73CE4469' +
    '67F077A5D965739C8664438EC770C14DB0CAED34E751BB3AFEA13B53E92E3CDEEB1AC151C628A44A' +
    '6F93D50C10FE5CE305EA9CDAF9920F7AABA65A65BA7038CD4EDFD633B15B8EE96595F5BA90C3E736' +
    '7C6FD8AE7356B7C870B3DF7CAD77A57A189472EEBC1634059928F0E44F8A113D8328001834AFCF64' +
    'D589FBBFF7FA6409E4BD0A08D96B751549B2DFD94D7784BE7943CDDCC41149EED29C003CF2F3678C' +
    '8258B3061AE592B5C46BAF4DCF3DED7DFCF0DF4D9CE1A37954356A7FD794E4AFB8A30EE0779CC9A2' +
    '8E72F0671D5E928E99E302AD7FC8864472004F782EA17249B803A134D998C8D0A52EBE3AF26C20AD' +
    '3B789C324456FDC1D8B432DD92A3A7CA8AC6F0EAD3F714C410D462FBAEFBE9138227172D58672A12' +
    '203228F3141BAA173EA2BA78B6152BF472FAA9A8F83643F19659AFCB7709F82EA91779B746DC8B45' +
    '22424222F71C9579E6D1050386A4762BAA1A38CC4A379F0744704CDB8DAF847A686DA42A26FA7CDE' +
    '58800E0701EC7B7980A10F7464A4BD6710988919AF04E0174B058793E655ECE6F31F41EE01C6C9DD' +
    '3D6BDB9C26F51A0013B7E2ABF343986ABD9617DDBE0D9B760E2238C4AE3CA620C515AA2CCD5C5BD6' +
    'D0BA2061CCDE43DCA83834F110FD123569CEA234656904C2F02F12FDEB8996BCE10792D7E447BCBA' +
    '1A91900EE7A1DDDC3FF4E05AE59A267BF78550C79847DFCF6960EF3D908A4D28FECA3800682581CE' +
    '766A64097A5BE69A6DD376637625C8F9E10DCC951056BCC843E16BBEB7C137E2D83680F96AAF8808' +
    'EE24FB6898742380ACA5A23C6EEC15C0AB7D6B4D32678005CC5E0F042146ABBB52D13EE3C7D33CE7' +
    '8BB55D8FCDB4F9023AACBF04D3658C6A81DA63E11C1F53FCB7F684A66994C55F7D024EACFAC70896' +
    '6A665658BA599D8D781DCF39F69A36278B6CBF500CCBA29211CAF2605FEE01AD47C3FD875C8DD889' +
    'F0C699C6C88B22FDAC5E5DA14ED1F6FD475D74B647BB23F2C0F66F091CCD70EE089CED548CF11CB6' +
    'A11D237C8CB4CB549B8BF9904F781865F17897F05B40B8B8C9BCFA4DD2BE7D23';
  S_PK =
    '43AD6560D3BB684667A559EE6EC7C816020E5B65671F270F2353A8C912B6C26B0DB0C2CF42DC747B' +
    '10AA3EBDD573B300EEA46C4200B210094F9512119A6BB837242762B2CE94C2467278500EE7B139BE' +
    'D906676663355B813A9AD9D3DB70F7AF2D785040BFD51208BD3D2CFB09EAF7CEDF77D1B59DA75F77' +
    '28F120C11898D9EC2CB22C73EB8F9436FF60524B56EE6B413030EB7DD10774261452CD8C5ADE75D1' +
    '967628078CDA77E2B1AFB83B9F07F6939D37FF54D5E10ED17FF8A3C21546A89F514576AE780DE876' +
    '1C4F2EA28828C69E38C730ACAA4CC8DC7DF63BA4C1525510FAE2C8E1B01812358BC5DFC01E955294' +
    'A5DFDD1CFF0519E20B8F74FE18854D80C86051AA5CC2FC1DB078BC785BF4BAD6832B8C269156509B' +
    '332038B4C3719DC49814FC6B6AD5360E945AFFF4D4AC235F56C7F7A9A872B518C1F0D48184DA0EB3' +
    '18F74EB84C4F324A2BD03704D2E2A59F64A8854C7AEFB2D3530E20C8AE8A487E6CBEDA645BD86A5A' +
    '83E77A6A22888ED8E43A7F4804C2DE187F1ACBA3CF55CF99412A7A59CF77A4A977724A72686FDF7F' +
    'C64492A5CB75921AD014EB727EDA1DFA7BD7ACE52FE292322F0BE0B004DCE44BEAA20FF06A7691DC' +
    '36405361F9240DDF2FD1A5EC422ED639505AB8E137B971D5729B11E84C040247424A51DDDBDBC43A' +
    'F261D038B0CD70D5BF44252A3786A26AF3FCD4EC100E5CDDE019F17BE6A64F820C3F622F78D4F56A' +
    '984122D6FA2D438D548DD87B9095F1FF02437854E2419A0316C33EAFFA0161737E476A9E707CC40E' +
    '78686D6A043DDE962B319BE2BF9F7A1EFF9EDEFD1B4CD07131494C084083BF76181E3EB139992931' +
    '4473A75E199AC9D5444DB0CEC07E625EC70C6864093961950987FB1E96DCB7E001209865D66D829C' +
    'D2E2B240818CACE003C9CC74DCE5151C65E59AC1EF6D495B0C717B4412C70B50CF44F44E648788F4' +
    '6BAF6F8AF3361F0E4B6119EDF6374DA596453169B935E1A3B875A6C1B9FE384AF961860514E8CF29' +
    '1D8650D7530DB42A46790649B5D8134AAEC33A41F0AB4296AE26203291F1C2BB5276AC305269778E' +
    '7F2A4BAC15B5A31A6B6B76342596D39C7FD3D1C518689372EBD20B667BE5EE2ED11BC107A7600EDA' +
    '1BE7A5DC05BB9F16D2B8BB1C7D8D10050207530BFFDAAE7B11E0615726F2E99CE99D6CA6048F9D61' +
    'B14F7265473EC2D02989772B3D7E212AA68D89374C6CAF7AB160C6C5E09502049C3D03738D700457' +
    'F706341DDEAFC6CA739ECFB4F193EA6B385B035EEA0F7BFD61FA776AF32AED6366E6C0642D1A0175' +
    '9FA6BDD295F7D18CA6DA1D48563EEE403F2F8BCB6A60326C481F12F8180B2B8117ADE61C7E29F525' +
    '4207C5D4657B82BE4EBA436752EC7DA0627FCED830C15F10FB8D3CD90B4505FA325B54D954C5B630' +
    '1DA72B262B226EAB2E4EE88226CC606B97736260ECB6D8F74A0440AFD5D751A90873FFF00C8D3E9C' +
    'A0975F303F7AC263B8FF496C6C8FD22E8EF7B587BAB50A7DAD99BD55D3B7968584F1FB21255EE22D' +
    'B56AF6034F3F13E659161A57CA8C9F2E87CA96BD7100FCEF8F74A8C6A1C92E2EFF74E2F5FAE512C0' +
    '9D26E0F3985D882401EFC54727BBC0F4E1110771A106898692D0C5A6997CA742846FA4D49E8ABDF1' +
    '23D92743E9949BEB6E46B9655EE698C23D74991C96067DEF06EACB981AD4A7A5ED91EACF05D374C7' +
    '4C443F3FBAD363B2450A1A47AAF2954D36E53B06345139138D38B941298982EEA84400C4DCC38F51' +
    '27951906EE3E40F75A5DFE09FCE9BB0143A5D5ADC3C402F23A75A423AB98392CA3A4D5D23D3BCD56' +
    'FF22C9612A5D2C223C7079958CD05175AA74DDD21B42051CDBDC14048CB43CB2F6535E2CA9F5B870' +
    '52F633976F4795CBA69D39F2481CFB9D210C9B0E9EFD941AC875A9A6C3E839EC54F55585721DE418' +
    '15DDFC05E8A58C97E2FA52984135AAB0931094FF8400CAB043C2A5E63C2942B7D36988C4ED9B73C1' +
    '1D913E758ADF94291A42743E4FB04C271ED5807EA03271EA6656CF967AB2595588B55F82AF2D07AD' +
    'FFCCF859ABA70B1707B722DA1FF393CC5BBCC02014C0D4500655577946DB5F95EF1E7657DC98402E' +
    '5CB048DCB372C9277FDA4D8F3A30C953822474CEEDA670D5E680029259260D91F8737CF7572651FB' +
    '28A7DF46F671679BDD507696B021C2C7F4300F3098FF9460582DB58E122C585185BCD091E7ACCB60' +
    '8F7E0C3558627484529A662C0528D419248B6565D32ECC78F7891DB5BB1984CDE89C1AF25F092720' +
    '5E734A7DFEB9AEE94F23F2FD11FB53EC768F6B8268E00E4054CD12EDE4832B07A254A4E2241854E8' +
    'FF2AE1E1B248F9EB1C77581CA2A2EF2D4C9171177A1E040F9D4AD8D0D0C6CD14FCD13B233794E517' +
    '04B6890C56BCE1B8CD1C9EAE6D59ACD91EB67B3A618D65F0F94E5458271E14DC6F6530AD0EE8B2B2' +
    'F0CEC14612E563338E241602B997EC4E62C83942C7F18DAD6841B1348CAB99A78F598FE78A20205D' +
    '88D826D2E163F6B628B266C187B427F253000E4EF99FEC0494A97D9B42E37EE613767D2651FB7CB2' +
    'B9E99578CE2D78B9C9777C954DBD1D7BE8B568F88AB42DDFD293BE28747103B052AD81D8F6254E42' +
    '6802516500111ADF0A8F27AE7C55D3D5DB86278FAF58B68A26D12B2801AC28EDA87AA5D692EDA9BE' +
    '08F7CC3E78517299A3FD9CE2A0A893E12D71062AE2514C465D399F165E4D2F71D1913D8B95396681' +
    '486432B090F0CCE86AA84B661FF22D4A56035E821A1CE30F33AFEB6C7B8FA9CE';
var
  M: TCnMLDSA;
  Priv: TCnMLDSAPrivateKey;
  Pub: TCnMLDSAPublicKey;
  SK, PK: TBytes;
  S, P: string;
begin
  M := nil;
  Priv := nil;
  Pub := nil;

  try
    M := TCnMLDSA.Create(cmdt65);
    Priv := TCnMLDSAPrivateKey.Create;
    Pub := TCnMLDSAPublicKey.Create;

    // 测生成
    M.GenerateKeys(Priv, Pub, '1BD67DC782B2958E189E315C040DD1F64C8AB232A6A170E1A7A52C33F10851B1');

    SK := M.SavePrivateKeyToBytes(Priv);
    PK := M.SavePublicKeyToBytes(Pub);

    S := BytesToHex(SK);
    P := BytesToHex(PK);

    Result := (S = S_SK) and (P = S_PK);
    if not Result then Exit;

    M.LoadPrivateKeyFromBytes(Priv, SK);
    M.LoadPublicKeyFromBytes(Pub, PK);

    // 测加载保存
    SK := M.SavePrivateKeyToBytes(Priv);
    PK := M.SavePublicKeyToBytes(Pub);

    S := BytesToHex(SK);
    P := BytesToHex(PK);

    Result := (S = S_SK) and (P = S_PK);
  finally
    Pub.Free;
    Priv.Free;
    M.Free;
  end;
end;

function TestMLDSA65SignVerify: Boolean;
const
  S_CTX: AnsiString = 'CnPack';
  S_MSG = 'E5AF86E7A081E7AE97E6B395E5BA93436E5061636BE5BC80E58F91E7BB84';
  S_PK =
    '6CD5B575E6B85BFAD904C66BFDFDCA515265C07FB8CAAAC9C58C14FA9189D491780987AFD99734A8' +
    'C6AC5ADCA5D8045291B96CDA23B1613A197464DCE9DE0C8D8BD72A8C796E8BB3E8DB58BBFF6F09F1' +
    '2BA821A3BBE32F23CEABCAA90B21CE2E6691F546EACD508C36570C0476464EA8BB0BAFB04CC89084' +
    '4AE02B5840BDDB011B76B9300C02B047C63F3D1F9455AE17CAD77EB65471F763C7FF8CD32359562F' +
    '2927070D590FCFB23D54550628A7C205D72699BE2FB51994DA6B92013EA9347F54609B0C73C28587' +
    'F77C4C69512D04C1BD1C331864E13502412A304FCCBDE18FFCC268E2DD828C63B7ECE53E9910F251' +
    '83FBDE132F15E666001F36FFE766B8BE36DD79A2AD29034B10FFFBBB7DC65A8C53570A26111A8ED0' +
    '271A3A18FF87B84ABE2FD0CB8BC51804A88992C4BE53C6CCD17F040D9B8E4DBF605C9DDFB37AA108' +
    '498F6E0DE60BB2EDD32FCB58EA7242263AC0CB5CED21E492D859EEF58A22F1025BBE8528E0DB1BD7' +
    '39F3B2C45887164B239F12EBB6C4AB4C2C076C79A4D1E15291ABC6EA882264EBE0D545665AF00AF1' +
    'CC27ACE53038B720BD19B1C845780107DBE713EE424A20F5D948E70EE5B4262E69B5360434C172D2' +
    '84893B149595F82FBE32AD908BA5663218706437C6FA3B8A6899BDDC09B524546653EAED780E58E3' +
    '0D6BD5761AF885384F17282DBE58AC04F027C1EB7E0452739728A52B41D008E15ED73477972EE024' +
    '624169162411B1912F4EBBFCD5DE6C5FFEC45F1821CDB523803D3DC2226F4652ADD955F57335EC03' +
    '44A71FA5C3DE0444F09B17ECD7AC89C82FF59B4001CB2E56926444A79CB480BF6799A21CA178E376' +
    '9A892106D0541D7E8C27579CA79F3516C1EC5ABA43ED87C2F8D650D30EC0D3BE9BD8F965354F3A65' +
    '718DC0E23627531D64CBDD648E37BD98598E2C3DF271B4C36A08AFED76890B30E0A5BDC99F1B4D2F' +
    '6E67BC693DAC6A6C4E3432BFE6E44BAD6CC1F5A6CAAA0CD6D8AFA6518CBB261D0D26D3D1F93C89B9' +
    'EFD042601B9A8CBB3AD41D4127583858211C1151696D97C7C1DAF75E196D21C9160081BC72FA205E' +
    '7328D5211A3CFD4E7D9377FBB765F16B5B114010FC24F15711EE6BF42E796585387E7F5338BB1D41' +
    '1A4DB6609F817C8FBC0403527D01864AF78F8E8A3F82780F4F59D82369E76B357FFF465D6C63E770' +
    'A5AA827DBDF1502E294BD4B238DF23722F6BC543824A0389240D03A7D451BD5445D2416BB186501D' +
    '2C0939D8B5989B4CB4686454449B8E9CCBF78C6F2E0A45BBE3B0DEB4C819E85D864CEFDA13637345' +
    '5F5805C3527E029FAA761A01FCEAE8E07DB3823E6A69E0CCE6E718522C3BC02738758ADAA07AD21E' +
    '4D1D91E89B41049CBE2F1B84226E31B51C0A18AC9B8723E9CECC83EBB3F0A3B4EA6A38052F7B2351' +
    'EE8D1E77CB0B2FE0299AC3236560E774EE1229F1A4DE52E1CC0DD580AED8F766DBEC048A3164AA1B' +
    '081A5C30CB6A6CF7772B3A2A62F33AA978C31649ACA05263F069AF17817B596381FCCA9ECD2C5EF5' +
    'BE0A80E5749F1557966EB9993FDBBD18B0CF6E83D51A67A43ABE78864ED73953E4F6C952C39F5D93' +
    '421A6CCE4EF00A7D057BAEBDE1AB5986F2C5ABF929EF662CACAF0C5FB9CB37AA2C6BD73C7C4FC5AE' +
    '5075FAB119D69CE931DFF2CB548E3A5F14BCA362C23D3F618FB6FCB3B154FA240B9D867AEEEACD5A' +
    'C6263800391E71C713F10EFC6516ACAD391E8A9FBEF561C5B4750E25635B448C171ECC9D003E8A6E' +
    '927C9F7485F446649E9A5C7ED0240BCD516825FB3E612E83492E0AE81BC4F9B8025424E8F2E50054' +
    '2B76B4CB5F9996E43A47F8FFF083E3C29683AC61A378D02035A7E690BEB865A362D6962711817811' +
    '21250900AA1038CE03485889248B57B9CC630711807EE9E1596B351B3589FBE7D8C1DD243D3E6ACD' +
    '18C489F6E6632E7640A9091FDA68478BA427C971FC247C6CB0559A5BB1C6AD66CB28520B52FA30A2' +
    '320D25A7F4294D91CA74B6F1B30CECA851F481E2C803028B7D745BA080DB43DC588474091FBF85D2' +
    '56784C61AADD892EB0F1CB59530C71377C008AEADDF805435F395AFCC4A89980BCEC41BEFC5FBAE4' +
    'CE9CE2A73E62061BCF0EF3FC0AD817C1949CAE4FD4F68B22FA94F8F60AB60792203D742B94C5921F' +
    'F9A9B1F9D937BF08093BBD868C14FC0B9C112C67EB0327C086D8765A33AA19494C60D704776002D2' +
    'FC552F2BD7DBC799A766F930C5DC0F897C0017110856F726C2519E86772826F5CE3B663FCE8EC115' +
    '3D5B8B45B6F81C25F051566EBBCC73C9355FC127E1C610EA440259E8415277B6CA3C855A7714D934' +
    '97A88E518F354A76DF82DAC51F44F97B1AD0123D35E074E26C25EDF9F5FD23C48289BE77B0B6C15E' +
    '56ACB0D00DE6ABDBA7BC287A0FE8241F9EC57AF74B47F4CBF7BED0D20A44BF5B0874BA21280FFC93' +
    '0353A2852E15C899151C33A9A0044B88A09F3576573BC66D3A8A0EA3990AD3EAB1297417CA3AE1F7' +
    'AB3A04500DC13BC0F05134F47F8DEE6726A0355689BF378CAC8C644CAD977827259343189A9D670E' +
    '2096AB86F4005558398D1F900B2005646A3ACFF7BE2C2DCAB0932AB954A8696DDF4647C8B5308819' +
    'A46E0FEDD53F5ED630D4172C6ED58DE221D677DB0476983A75F554B4E3A740A69E21198B1A3F0BE9' +
    '28686221B8FF427B530B86A96B67554A27179EB2042C059322204A65E3E1BD395A3CBD902451EC3D' +
    '251C9C086E7B33EE8E3EA64ED958610730FCE7F2BDA38F2318CE5DF71E6E15A5';
  S_SK =
    '6CD5B575E6B85BFAD904C66BFDFDCA515265C07FB8CAAAC9C58C14FA9189D4917D7ACC758C92BAE7' +
    '0AB72903A2D3D4F5EDEFB2697A1430C5810BEDC137EC87EFFD2F79294AA74EB707F132B29820CD58' +
    '093C1148FE6B636F7F80C72E777CCCE6E67A5CFAD97341EC995C734800363A1CE491F27196D43221' +
    '05FF27AC79F5EFDA6028613635741066431577625275240775631421041857164856172726870025' +
    '52873312361213543650013127757675844176443225044383051110810818760265006540127134' +
    '80234146182725452405316353262722273863410527682651124313617146446468763236572883' +
    '68385134128205321528025474023506024085032444083127257307021732667407461177726523' +
    '00570004633206265338265677235466846412613611721688324168387324470001757618178840' +
    '01281870505407105812083630737534234530861250140743370224166472407765428621832254' +
    '53385182526476214180527386283154231800360221755387408303387123147433480555354064' +
    '68343618578723627210132213151543778721142836338781071485708651271227711682567888' +
    '32101555628657835804815235874745320114807487105725084663633033803220002386732063' +
    '12581001487286248647050271141668161628543512147267550386275421181032142703020520' +
    '51517843085158045208008158156212712436722140358400447436786717215464352804283606' +
    '07411178157452818316372232228180847276018025186046848421146613878205377117430604' +
    '35681375208200043748231243683135031887468574370846435103020132564675432587287181' +
    '35314552350848750665704545438303155627872625251767302275260286767538816048617475' +
    '36478534804518058527772888625364743737287676713036472565548446734032381158814262' +
    '07687338861838607870731705072055017718473283064863781725578214305716354841832367' +
    '85640812264787001144552041321226082508423014113433772434356426247544802437024174' +
    '02034430237738751184615413645850651437310878848363433552805188824680247181748566' +
    '62231877411232273575882278503637071445047660647601061448570702766728170880116267' +
    '10510010308324482073477014546652708031305405153411317224571663238403030664670025' +
    '16864388122301818603411120171217517454770816548534057721877862545820644757227243' +
    '57821284580830033608861344165638847121366380421860180375410601338541424866174253' +
    '23342085336261400561356488420782018837350743365258453470541321648000371455615527' +
    '56104552631133011157715447235321406074057154263833247258731718347077202106654146' +
    '54482844775264046825038723174370473101277857303361318503026318555216721803265524' +
    '27232054062668513672538118563712324188851252363848187162848844770101425858714365' +
    '13052173017334732003261886333314274132286417376656676314711082686504118838381531' +
    '50788020563700424777450648102782480343261522810357211344034385608408463423741051' +
    '82485205511272523366067366815575443832031117176308532056565162544750224215558811' +
    '70337323534030806477563736665682820467542048717825503372863768782825134682228448' +
    '43416423412834176245807337287262143454763160504644733726778323616351434462237047' +
    '38488337035225328282663573663543382176618000123757725858056388763047508731061246' +
    '83373681371476321454728058642283077630656787704536321600871220523360851484085556' +
    '12124308056334555688772683370127655208724021041003400106008323162736751377541515' +
    '62385175812016001303620126160845215651205172667800777483185035365242481478081677' +
    '70656334505577640785201013333135BAE3FEF07D33D7B9842E4FBD3F6D285AA6B47EFA4A065158' +
    'F0657A04BF437A7839C426766CC9C0849273F705D9D63DF0E7DAA9849A312878BAD7D9FE7A9881F2' +
    '8D7B8646917C10C4FFE8C51DACE5BF6D5130EA118FAF211990D6AF646D26672F1AB2F69F2660D586' +
    '8D95E01C8122DDCDEF420EA8815D4CE0B3D6D85684078279089A3DFD7755CAA106A2C6B4FCF17D02' +
    '9C85A95D1DAA7AEA75FE513C1CD73E8797A12D1494CA21FC317F9F0EAF256A61708889BBD22FE29F' +
    'AD2D092E781E36F62DE70A7474145E2EB968004A3A6F46243D05682B830EFD54386753758B013C3D' +
    '56F6810F3020B38F53113B6F686F0A1278E51DF2984EE3A6BEF27E8F7D1939CC37EA92022ECD1408' +
    'DCBDFABFD590325F7FC52711B7500117F0098C96143EB1A2EBFED3324FCCB6BAA53DC4BAC6CC16AA' +
    '4622C94EA05B9513F9E6EB29C48682F906F81272E96CE317D869687F2AD80022A92F27F5411D547D' +
    'A54D315EBE877F9DD20A74FE2F3FDEEE7296554CBD7A995E4813632F1D12965A8AE65FC3B9E8A314' +
    '14A02CB084271F15669D3B97273D8D5F4B437028C0BC0CCCEC3F4A5C7B9159E377C9578992C364B5' +
    '9D85AE33BA6040A92B651BCEB410B45F5CF5597685256EDE85089C10AD26E1CBD5424E868ECC250B' +
    'DE1924613636D5534EE62C21814BD47CD558489AB9431B9594A08D6EE8252CD00E252F4DAB2DBC15' +
    'B946E2C30C099AA9186ECF097BD0983E788A4C1393BFAF1648C7F20FB9D663E3A5512940F0069CFD' +
    'B6077BF3D68770F36546BBF94439F0C298D5D5A0766DCA829CCB33A6F528746D370C1349C39030C4' +
    '62E03C8F7917C7E2B2A270FF43159C786BF1FBB9C16337F3799E2FDB382A33CB296CF85C5B947AE8' +
    'A8FAC49EF4A493FAD541C81BC6C91E9821D830B6654DCC87A86EEF155DDF00FDEC305EEF1F78FB00' +
    'F5085058942A230CBACE95388F879729CE08D7F8B8BAE1E255D9B987CEAA10DD72D0EDF01832875A' +
    '50FA34326EE0FE91314212A682451CDC1F4FE029D051E26678C1259E022A34DBA1606A445FDB38CC' +
    'A2E3BA566ADF56CE927D66A662DC5C27CB4152BC3E7182B36EF7A3E67F02A19AEA43A3088E7035FE' +
    '0C92D4BFF9E75B7046BF862979DCC19C9B7E7434C4E6DE990C08FCC0F1DB9786AB53189DE247AD40' +
    'F2239E378C19CF66B55E596AFBC90DE4917D3D4FD2D0900EC361F40A332DC5B05BE1F7FA7B552847' +
    '36F4F7BBF9643D8C3E592FF065732C0AEE5C8F15FA998D77FEADA4F1743189C714149788B7E2CDC9' +
    'C34C64C7217BAA52FE288DBEED183487B7E5D135FE56B989AC619F6A771CA8C6DA5D780CC02BE40D' +
    '2DCC55E4FE4CA9171EEEA766C20AEAD36EA72F756D2E76C4BFA730F329CD1E36250BE6E79C092377' +
    '9A74C8C792E98506D583E71870EE53E190333F830E6E276C84DA25974B3C6D7BFFBF1DAB2A53655E' +
    '005EFC4C29392C851F14CAAA17F3DD53CDDCBAC0280A47ABAF1F466C9A2B960A40501D100A659E7F' +
    'E0B9C6775E8ED966AE3ECFE2D3C7E90EA8ACA6274BDADFE05DB229E3564E5647FBFBEAEA9962231D' +
    '3808360565785B0C3B3F713DC0860A897EA43605FF11514AA26351021E3D469605F49522B6B3E572' +
    '6FAECD59881B32F5D5FB609ABF5F282D715B8FD88FC87B280732142EA8FDB0FE25D33758572493A2' +
    '07AAD4D29D36DB979F3B34606F5BA3C70F2DDB60DD84B056864B59DDB3CC651960884BCB2D2F3490' +
    'F3BF7EC981F9F5C11D061642018F68DDFCE55BD59E22314349470EB70BE3F5CC31C6ECACCE6601F5' +
    '3AC990139385653345C335A54E16B91E5CCCD56A182981EBDF05C22A2E9F51C848FC306D9B78E99C' +
    '21A1E094E8F4D8D5AA412109FE79DC58C002BC75DB3653111385E2D2C99C73A8B7D50233FEFF2FFB' +
    '4BE26A04D85D05FD851746AFE0517C00EF3D930A66E119EE45C5DE23E6C2B050D7ECBF74C13BD0C6' +
    '108AB2CE576101EE88DD908E725A6CE1A1C090C6A8570174DC73F08B5139C04E71064A43DAF9DD2B' +
    '4D1812390E97777CE6CA8F4FE547463C866D02DF77A9AA540E4AA7596A359800A835E4E2D23534BB' +
    'EC0873242C14D95C2B14BD33D4B901CA4D4B12BE2CD53C7FA6367BE214A735ADF5844F7F3118D0CD' +
    'F0ECED6095E365E4D18463726EA9FD9943C7A57F65B3450ABF855833E5AC1D97C735DC8B90276039' +
    '3986782F7B0833C139A86F0258701F9A65B07DF842A3F9C7550A91A94523B1431E6919ED08A2C4CE' +
    '0E764907B745F8273B6B003618085A34A37779FF9D971DC4138E28335D6BB9B4808C0AE2DB039259' +
    'D46C418366A8D81BF1AE0194E8E88E471473BA163C9FF93AC4B0E4236CF4DE6B68D5A67BCEC87F9D' +
    '9C236EAC2DCEA379BCA0721D241835AFF9CDBF5289148155D077A92D67A5916949A972EC12378B01' +
    '5F4F60815AA18DEA20B89F8E342458DDA03A7211EA8F1150A486A5774FDB17B81D0F806C9B3CC4CD' +
    'FECFDAA4D3FEDF6191C071FA30F4DD738198AC603C14BD8EE8E00F8B6EB52E3C52099ED2D338B841' +
    '088563117FA33909D3ECC64A4DC26A09FE2A52269AC3142B1061742D7F294DC78E288A8BD6CBF342' +
    'C3C8CB75D5D409CD6C7545D12E21DC2D5F4A0B232E50A2E55E1129442740521F6D380E1903FC00B8' +
    'FB16D7C48FD1813A729C7B1B3D20F01F86F2C63FDE04C874800BC18697DAA19279DA9C35971D1CE6' +
    'C42DAC2F488F07BE41DCE4B0140BA9C15CA6D077291C7494937DBED0028FAE8E1BD690C599235827' +
    '9C671BF8CF0C230493604E0F75D6289C71C23A72EBE49866BF09072E8BF796BFF8BEC5B1426DF55E' +
    '570EE4E48EE510D511706870FE3EC320F63AA1BA2F974FA9A97E3464C4352EBC8C1104114B433890' +
    '4A2103ED4AB8ABC915D9397623E0B0ED842FC7B7C3DC8F890DE491CAAA7E5DA53F5A45FBF4AD4E85' +
    '0380FE7784EF174A9EA6F7983C3CC999559B5D8AF0467F135F4DDBF7E76CDBE8D89163DBE2402B5D' +
    'AADEDAA1D8D0526DBCF7A4F68BD2D375B4E0F12148C7FFD5C2D4BFD6A09FB47E66BD0B38CA3EDA4F' +
    'C31955E46DE9B5D8E54C7660CC1DD46B16201FB44453687DD69B7B3F478CD77E5C98168229D259F8' +
    '4D4F02C290B01BFA372E471D7E66BEC67FB69FABAB568DB55DA1877D73749593953B2EA002BF2377' +
    'CE235E18827C9C954B28DAC0B5650464EE005E906748E44382DCF9042E20DF3E80EE38D375043E45' +
    '7AF2FD13CF4F5C9CE98501E1AB1D97CA0EEC80D79B13D80ACB423996DB5CA315951EB2249F0F1311' +
    '18DCACBB762A99D1874ABBA59AC2828C1B8B9DF1043C00D267D9D5CAC1334DDDCD625F15625C8A28' +
    '7835867118709CF4CF7516FA1CF5F2E7A4404C5699118C52426C24B5BC45D3797B5C00605AD6CD1C' +
    '302CC47451B433CF9896FCC23F89D295EB38B7D5D652A2E565D7BC2AAD8014A6FBA5A4CA06242C6B' +
    '4AAC53FC3B464892DDBC6D0D8B4F55997BCE919690D23F56B7A8D552D5944A2F5385BF6021229421' +
    '93ABB3FA275FFF850FC565532BC8378F5059E3FAE22C4319D32DA76D99312274';
  S_CTX_T = '0A8B9249C5794768EF495AF6483C80047295783A33DBB8B1';
  S_MSG_T =
    'A54CB4BF79DDCE7D7C446F972A849DC3C75917E7ED52777590D49BA28A8171B107650CC74348C2FD' +
    '5AA5327FED13BBDF514ECCD477F4D5B213F8083CDB2923AFC35DABE663EE12932E8FDCC3058B6D991' +
    '176CBAD';
  S_SIG =
    'E47D117DF51F7B54875CEA55017BE42F1DB20373EFC790331516CFFC243D8C18A542DD5EF98D3479' +
    '758B1C598DC33E38D1497EA9A7AB23D79BBCEAC734F25C2C24D0DF4AD754DD85A4213D429DF2359B' +
    'CBFDA28A5A3FE34017F24405F6B84684A3FE93C8C782C2EAA76864F197A3EF09A1F8D9F47BDB0343' +
    'DFDC3672D6EE48D30ADF6E8F7217C9DA9C65F372706B939B755BA8BE1B5F23BC317369F710FB6CBE' +
    'C0DE00F10C0E9213020551E3FB6E7F9067C1FD08440FDA6664BDD50228A642AA6237A815F2900B21' +
    '8193BD36BD04EF8F4DB9B1CF23FF9E685794B37C7B61B198A393AD1DF702DA8110F3E0CA00C12121' +
    '267D721F226FD5D6CD1F92A455F6568E420C1F547BEDC5306E2B71BF8CAB6C58E6024733649F03C0' +
    '379C9CF20365966E3489E0A15727841D48E8C4145557A62E6A882F1DC2C0ACF0818382EE8BCF01FB' +
    '70D0BF4208828619B10B3F0CEC9041A8F83F6164C28452F053B466261766AE4FA9C3B45531134BFC' +
    'AC3E6F26E34EB8106F08C1CE7100B4AB57B6BC597B2DC3500DE9E7849E9C5F0BD32CAFE20D3F57D8' +
    '3B13A252A3E190A92710C29DBD48E863FDFBCEAF4656E048CCA6832FC8867AE9209DFD571B3886D8' +
    '2D5FC19652FBA1235D48BFE1F5B1E0DB61ACB91872C81BC56266C02A98CD7DE70E57F3BF4892A740' +
    'EF3E99D24FDFDAEB653E0541479B5EAFEFF9C2FD7D5A61938D9A4AF56444D0F84C9185D7C9B23D0F' +
    '96A50FFFBD491A550FCA2CCFCCE5ECE6A02CC5CB2661634FF4E08989AE7E75E6BA05B6A92D89E3ED' +
    '7830C3D08B277B6494F6C2EC807D5731B973E0AECDD6C98736C226918D64374827AD3CCC6F170A76' +
    'CC70B05B3BB5C0632A9BEDBA383EBB034DC8726284D9ACFF9D468350AB7B9FB5AB8CFE49D240DAC4' +
    'FF2E971EE3B40AC3131055C29B35FEDAF567C3D4823E238AB3EC5602C27FC7C8BA6454AC72F713C1' +
    'E10E47696EB104CB38C32B1BD15DC746ECE463245F1C754D0CC490F03CA647D6841421B879FAA1D2' +
    'CD0EDEB5B811606049F83E6A54CA29FA68E987DC76E21FBB130AF53CB1E81F6B33C3F236040D544B' +
    '13DABC6A6282DB3BFC8127258F8714ACE8B22D3D19C5039B2B6F380D6D0907534096924C6446A771' +
    'CCA6997DB2695B25F1469EEA7620CAC7304B272BA784C9FCAD29A5168D8EAAD603507DC0A9C77077' +
    'D8D60AF7CF96AD78EAC512CDAA74630FB9A2CE9C59ECE85D92E8EC0E45B3D0E2379EA4F0A9148579' +
    '745B11939E497DFA8B326FEF9AA4E6311CDB033A1E5AC84803B3FB397C4A72890CAC2717D6309771' +
    'C137E60A1E07D296840401CF55877FB2D6147BA60226AFEAC149E1E31A69BAF8CA114634680A741D' +
    'F29A8A13FC3655A745D07A79DAB9FC4DAE7308175BF217A7C304F4CD8139D550ECEB8FC189572221' +
    '41F7A2E4BF3015FBF07A6D87DCABA68F24DC9792CE36226D095706AA728964140C24EDFD85FE9164' +
    '075D015D566FBF2CA8464F5689E815FBDD22AB0758ED9AA5B34B40395E2A6B3097A808E9F28BDD6F' +
    'ACE3F87F1EFFFAD42898D8EA1B74CDDDD2D34BBD211180DD541B9501793978D66AD3AB3D7B6AAE16' +
    'E43C220A99CF41336F1956C74BE21F86778577B355D426C25D4739F84399FEA22E4F56326BAAEB08' +
    'E3EE32915AE24894483AC9B252BE5544FECF11DC3E746DF5EF35B43CCD81498F9F6BEDDD62531CCC' +
    '29641698F72E7AA5ECF63C239187BFFF798A8523BCD2EAD03C486C94280B5E7E322925F69EB9B867' +
    'F42C1239C9D6F5B9D9F4D0F8F87DFEDBEF48E03EAC17A23F976FBEB76638FD6A68AE7E4D7DE1D9C4' +
    'AC5249E204AB7D60F02ACB2710D6A993F4A65D01C653A90EFA290740F257A40CF1E95F4A5EAF60BD' +
    'D90E65EA9838D89B383AD94EE5B6A966507D80CA9920291CDB286CE36F3285F79A7804EBDE1D3058' +
    'E3CBCB485F4A016E1CCC6BC0BA126A1BDC80134741D26FC6BA3DF72350437E1804B1DF404566FF1C' +
    '7C5587950A8833AB5CA0DFE2A639ED02CCDC7E25FCA9F1A3B3ACB66EB25AE31A6E4B126BD043B20D' +
    '02A7A85756B31D0751550D18E65C67AD43B8375F895365AB87FAAFFE244EE2F909D47613BFA3D969' +
    '6DCF1EFAC32427B99ECA020480BB6E207BDDC108052FCCC8C815B73296B51B3D3A1848CFC0E1F64D' +
    '012740291AB93B0FF624A58E557D4B66AA28BA874C15A33A07A1ECFF6AAD72693735D905B38AE1D9' +
    '45718371F8A496336162CC26D2975933A56BCFE8D6751CF81815AB47F4F5EB7689C65ECAA40EF685' +
    '8074FBF2F6EA854FD03E32EB7D47D0D1B024534EECC166890E6C26AB24F4545BF9EF7BAC3E431AEB' +
    'BFB42E5F1E941AA08D2178531F1334F4C30165E1460F7E45EC16B04EC4DDA388BCDAD47D0F1C88AE' +
    '49C1E97E0E709A7CBC7E71FE9C60F01629644B803F3B4E3FE2E2EBE53FCC9BACF146FEF23D71FC89' +
    '590E8B3370CD0BDF75EAB4058F300574E960AFF27121AF82D2F4262F13490F30FC5308F5DC9FDF3F' +
    '88B52DE8B1F503E4B56548DED307CD93A73B666E09D24B04A1DC24B8EB7287C3DFC23E19406E727D' +
    '196B7DC053A7FBBB14D073E922F45F2311ACB6D9E4D1ACB3DA2186A41BAAA0940845047E91B9E314' +
    '68A1AD549C0576913DFE834DFEA891A556993A079B14C129C1B273BDF27DAD2857D906C8AE2ABCCB' +
    '967F55932C3FC380A1E1784811D0C8CADFCD621B16E24BA0E84AC5F099A5E77B0C6C191F6EEE50F6' +
    '6841729898C84E9DF25D985B748D8199E303E9E36829CAEF0ADE11B624E725ACC034CD7ECEACA81B' +
    'DC208BEB05B785F2B35D4FBDBC62F7E48558887CB45ADD39F18C609037EFFF392AAC7388763BBAB5' +
    '65EF73A608E3E862DE246168DA7089F4808E3CF4A4694711F554DF12C885EE028470F9C8A9E1943B' +
    '3D10002AF631C8AAC815E046D8B73822EDDC2F3DAAAE89A479FCECD946D0DB9CEFEED3C0358E9841' +
    '2EE91C48A9414BC58BF1DAB4BEAB35B2F13ABDD35AB3BC3DB60A42C3483412B6865C5D7AEFEDB79B' +
    '12BE4DCD70370E7EBAB739168299670A3CF4E2E9BE2EECC864F473FD7214676F416CBEA52E743F9C' +
    '435B191BA27330919058F3B27796F6D4BDD9E4188E2FF134EE9972E0B47C614D2C1F5CF3326BA428' +
    'EA3E15A141957235300D67389385F3BEA45454EE2332E5A1FA021DFD5ED6824D1DF0FBE94A6F4AEA' +
    'BB197263875704B891F20B1AC47A1D2F598A769E754A865BAFAC8530A726FCA1E0FFF4726DF280A9' +
    '549BDF7B59C032B5E70FF0F0B05A0789CE6A6039C5562637EBD8C2E6EEC26BFD4FA599287280BF26' +
    'E68487E7E103EB5F9B97FA390943774DC9F73328E50947471976B26EA7875CA42A672C976C575319' +
    '725DD41E205466EA94F227FA77242C6866122F650B6ADD3C75E2CC8EA314F3874E3A311059550E6B' +
    'D1BABE2F4FD4C5FF9E0930C910E8488C821F8BE09C2CE7278996D31A708F44D03D141999B30E99CC' +
    'A6B91264EA257C5ED581E7A92925B5EB477E0C7BBF429B9DE20EB7FBA0A6C258CEA8B3A1C4AA7B81' +
    '24C6ADF192C0EFA67084FCFBE9D26D309C52DA45218E8637355065C605E3F688A1DCCFAA7F5179FE' +
    '650A8B002A5BB862578E538F60925C87A8AB68A0780A71EB90A0E7BE9916982C827EE38364BE2E11' +
    'FAA771060DA5514CFB11E175967016EA79953830960F9A59F132CAC4CBF7211FE216E8CB4E1C4D5C' +
    '2DD8190E4E8B7EDDB49AE79D67CF11E47B2243E60C2BD6F614E5F92B625E941E8BB73719056A377D' +
    '9D4C3063214B42D74ACE8502156A0562E6A011BE9427DB957654F04D25800466D6DF157157D3ED07' +
    'D8F8CFC8375D742DB0B2ED0CB3383E2F7ADEC648F59A0B52F3C32673A2A164479D8B2E151535B3F4' +
    'C34BA4078D5A4713580B103E1F5DA08463EE9A727B8E9240CEB96FC12699F0065D60C896BF9CA134' +
    '78313413C94BF3E2FF46E78A00D6CC80A47290E0F35B0CCE780E27FAEA8231E10AB52F2E749A381E' +
    '6CB5381AE43CB564D05C953B1581A4E7D2D16BEBB3BCDDE7CFA85FDE822EA011479D110DF5404488' +
    '6D1668F0035FA3CC906626505814D96DFE0DE6CB0F8EEBFAA720918270E55B6ADE2BDF155082EE43' +
    '754B97F1FED36421691CF29A2E717055879AF86FE7E633EC0742E958B23B3FECF5E2BD110F73834A' +
    'A3B9CD9379F0503B435DD67DE9A253616593408AF5A492C40A97025C76A006877DC5CC3E98BC4323' +
    'A45C0F96786ADD841BA823FEE1A989B2E0A329954DC07542992D5B8747B424C94965833B89E795A1' +
    '08EC3BC04D6CEB1804F2B22ABE89157758F0AC3EC41526E9D0D19AFF285C7C33BDB105C7A1FE6F22' +
    '58CB43F6A7CFCA44EC3AB654E3FD6F69269B782D517C3700F9CEA95C95DD0E17E77302871B4F212A' +
    'F750655F019F85249FCAC5D0EDA33651FBC839B6CE4D43B62AA2AC33ABB9B74F95F65002E4AC8047' +
    'AF5EDA3DC5463832F84B328CFDE01AD4CA0FD17C6263975D36490F3D51D93C97AD9B7AF04BB426DA' +
    'DA3206C67B2575EF318CCE00D89B51654FAD2463CED2F73C3DBA5BD11CA84068ED37181982765DCF' +
    '25C7A3E674859C28601E0A0C10AFA98BB1CAB2C6834694AED72F276054CEDCE16F313D0571218D47' +
    '2912071D5B8BAA723D4C5A65A1A2B2C0FA2F385B66868993A1C11C22323B4F698F949EB4D39DC4FB' +
    '0627282C496A82891757A2C3C7D9EEF1F300000000000009121D202831';
var
  M: TCnMLDSA;
  Priv: TCnMLDSAPrivateKey;
  Pub: TCnMLDSAPublicKey;
  SK, PK, Sig, Msg: TBytes;
  Ctx: AnsiString;
  S: string;
begin
  M := nil;
  Priv := nil;
  Pub := nil;

  try
    M := TCnMLDSA.Create(cmdt65);
    Priv := TCnMLDSAPrivateKey.Create;
    Pub := TCnMLDSAPublicKey.Create;

    SK := HexToBytes(S_SK);
    PK := HexToBytes(S_PK);
    Msg := HexToBytes(S_MSG_T);
    Ctx := HexToAnsiStr(S_CTX_T);

    M.LoadPrivateKeyFromBytes(Priv, SK);
    M.LoadPublicKeyFromBytes(Pub, PK);

    Sig := M.SignBytes(Priv, Msg, Ctx);
    S := BytesToHex(Sig);
    Result := S = S_SIG;
    if not Result then Exit;

    // 随机生成反复测
    M.GenerateKeys(Priv, Pub, '821229E3226BB7A7774BFC3C3593DA9C4DE76255374CC4B13F9F62860E0C3E06');

    SK := M.SavePrivateKeyToBytes(Priv);
    PK := M.SavePublicKeyToBytes(Pub);

    Msg := HexToBytes(S_MSG);
    Sig := M.SignBytes(Priv, Msg, S_CTX, cmhtNone);

    M.LoadPrivateKeyFromBytes(Priv, SK);
    M.LoadPublicKeyFromBytes(Pub, PK);

    Result := M.VerifyBytes(Pub, Msg, Sig, S_CTX, cmhtNone);
    if not Result then Exit;

    Sig := M.SignBytes(Priv, Msg, S_CTX, cmhtSHA256);
    Result := M.VerifyBytes(Pub, Msg, Sig, S_CTX, cmhtSHA256);
    if not Result then Exit;

    Sig := M.SignBytes(Priv, Msg, S_CTX, cmhtSHA512);
    Result := M.VerifyBytes(Pub, Msg, Sig, S_CTX, cmhtSHA512);
    if not Result then Exit;

    Sig := M.SignBytes(Priv, Msg, S_CTX, cmhtSHAKE128);
    Result := M.VerifyBytes(Pub, Msg, Sig, S_CTX, cmhtSHAKE128);
    if not Result then Exit;

    Sig := M.SignBytes(Priv, Msg, S_CTX, cmhtSM3);
    Result := M.VerifyBytes(Pub, Msg, Sig, S_CTX, cmhtSM3);
  finally
    Pub.Free;
    Priv.Free;
    M.Free;
  end;
end;

function TestMLDSA87KeyGen: Boolean;
const
  S_SK =
    '8C2D4B1CC2A8FE9AF52F21ADC92D487F90BA64BA71EE1D0F4466AB2505BFE23BB6F3DC2437732BF8' +
    '254F147AB4311A2C20B57D71459B36D3CA6A85A6E7610309C7ECB40ED8B7B0639494629A1AACBEAB' +
    '42FABAF354C56325294FAC93E11B44EE2B8D1F82F4D59E0869061B88B92600558B7FADB3402EF1BA' +
    '94095C9C326ADAE562B049DC366A03046C5314324B2044CB182C51242E1437480A118A9496090C18' +
    '8688100D8CC8009A34460B27520A98259316889B42690A9545D2226C11173123B704134665581849' +
    '98202502369223C68C02C149E3486D12354A9CB684D1262193086259366822C42184328CE3220D01' +
    'A28018C24588208414C968A2429013C269122172E0085124094CA3486C8BB8290CB325034672D442' +
    '111B218E94B6289A448C19050E0A214E54B6041C960C19086C4422500908240A490A22314164A244' +
    '2496894CA00402A60C13B2655802120B2725D2940189484ED9A481884890D31661DA308603454E01' +
    'A56022090A22026D581821C428319B1840E08430A1B05119847181424A22322C9C2420C326511805' +
    '5282444C59B840D4A84948A0044038050B06824A8841219120244961D8144ACA102E1B0452090762' +
    'CA14420925029AC8508A306CA1084E0B83915C386D9B824C14C180C4B06882422201878419C69182' +
    '089204B08C01842863024ED2162E91944D13A9249A966024A2811CA7000A21252099689A4825A444' +
    '6214308E99B461224060031206988610C3308D0BC821610831D0B81023B98103468D44A004E2846C' +
    '64946442C400CAA821DB068DD094205AA668E3022A43266C13A444C90609D83261020449001644A4' +
    '34050A298648906C94B485248029DB920D10C52C9188290A47618A1461D3B68122000491084A6480' +
    '2C43144E83940C0AC28C6234500AA204C9A8201A164614160D63104D88007180246D11242A541442' +
    '0CB80559468252006619B1800B160960280E8C24801BC641D294500231699C80419C1284E3469214' +
    'C3044444260B04091AB38D602469C9A4912331409A940D42948053B22C0C417262322111A52C0326' +
    '2A4932025BA60D1B288422B884DC124AC4402488084C9C104291268A103549E2084CCB2005091284' +
    '18120A0BC8701120821CA40801B491CAB600E2364C1080299A9449C4A44443828182388A609441E2' +
    '4630C2C4241AA04802A74519470E14175101358E01B62923334E10A88451282D540800190786DA14' +
    '6EC8207292042CD0222CCC2426D13406030010C1360952168A8828421006416394304BB429631092' +
    '49122204178C0B084103A660001886C924702142320C3632A1345214226A00256E0A1351C34411DC' +
    '384C11B865E006300CB521E048124316885212480CA1090002246236100092914014321221002215' +
    '7220937013312C09354184C02D20362A42C82199184020930952A205008068DA3291619230802831' +
    'A018029A12821048081142909A824948220C1023809C0205DAB220D3C65012B04151A02D88C82944' +
    '423212C86960A86143B66000A551CA402E52168808C4800B81018A446C42A4804048110B123004C2' +
    '00C226319AB0059A4612C230302349245004010AC111D0B24D023764193988D01040624285D49291' +
    '93A20DCBA25060A42C8A2880E40000229020911011C4984C131652140851CA1440CA36656324428A' +
    '104D0B452814380A922025124986C04689C8146C81128C1BA7444238659414105004718C08216480' +
    '6082B8008CA271CB24289298916246329C2608449690A3160D23B94843424209152C0CB8459BC430' +
    '980605C3A409CBC66089C04480244E11B5455B40700017410B974822148881280891906803A92D02' +
    '316623280C0B4180CB484D043221C1466500272090A2001AB50D44301052360D10080DD8B2642488' +
    '64199141A0441082404903296051124D5A489102200E44342D0CB37100449019B670E4A84C8B0026' +
    '92948402358083226A00826D1BB121E31649DA866920918560B26C00398623154214808883482D20' +
    '454A1B17425C18080B492282028211188DE41842E406290A1271CC44449C4072E4B8900285241927' +
    '2608260C911864A1A64DE4968114B74889423120874D5AC0600B073154C4890B404D83C445E4460A' +
    '2303859C80052312911B38314A48691944500BB40D831622C4466D8B36249894098A244123832961' +
    '2061A4986D1C37040F4584659CD849966586DB948FF203C917C0C6F48C9BEEC1CCEED4DDE700A592' +
    '5E3100D9EF417D271A3371AE042DAC0D7A6C9D9F799E7950123B20B161FDE51F04C54F48943B2E1E' +
    'CD7A36724FC6FD407891B8E54337831A07B3FA851547905E066CAD46B5E6DA481B411927FDD0E187' +
    'C43500271C548B123B69D38EF2A5A84A8C4152C41E51FEF7B9682347FCEAC3233A0765A8AFDC4233' +
    '6D622D97210B19DC6367317A109E5CE269B02B3D7ED49E48E6A6A7A4B4393DD15CEED755953EE6F9' +
    'A066270A662A43ABA3126F0B3DDE71AA39B8B65A4FE52197C246BC5D1F9DEF0C936BACD7B3C62B78' +
    '5C49498C58BEC34EB97D7B9B3958886D0A71E43C249D8FC89801377C5953524C06951A432E3CFCF3' +
    '8D7269CFB42586CDC83736DA73E6AAD025AE20349B48ECEB87F94F9E77C698653A725F2715B8E296' +
    'AE9509FEA94DC9323E8BE7E25E88EADD1167DA145B1E68A1514BB17D45D6804E6B9FE9C245A6AFEB' +
    'EE918507952FE168CD4E8AF70C222C47F5584FF1537115D2ABBD743A53E689EB3D585D3C3E0A3472' +
    'AB0FD45E0CC835629B1068AB2D398FECA7A6332C961834A9EA62919B2BDBD8172F7E75E744E555DA' +
    '39732854583984306F7B83AC180645C3C4C3F550521E914C28914C339EE3F84A3E20D2B6259540B0' +
    'ADA3050EE18C1CBA03438AF54203E912D9C5F4408BBC6401184C8034BBD4C634F029A79EDA89044A' +
    '8F022CEFBD018B41EEA419436AE5C62FE2F9FCC43D7E6B7115F4EB22A73898B28CDE5177BEF3CE45' +
    '917CD762B3ECE8007D1D24DA492845BB7EEA4D17925FE654D7EA3BD72378803209CEBAE83BEB3120' +
    'CEC455C724442984E925ECCEFF2F470A2E84833DD3032DD0C80133ABACEF0F6ADBDD672B4A93761D' +
    '21794E8BF0F3F463F0A112EDC0212D0E89FC141E35BDACDF577D996913FBAA30EF960EEE6122D100' +
    '7167601C1E929A0A76EE032F8D4AB7130583BDB5D64783E3276DA79B12EB1ECF42C0208165C1848D' +
    '842AE62A56A8EF604346885C49F69B91F31B8142238650725B3F042BFB924B8EF19C9EC3249B0E22' +
    '138A5F0DA221A694FE8A86E4F54957A4AD3EAD4DF3A1FFCF7D9C33A1877BFE87F653D22FBFC0304E' +
    'F19DAD418E8D308937E9991C707E22DC6C8C332AC39A728D30D3F3770CE51B9593AF943D8B18DAAD' +
    'F59D4DC03AC595BFDDFBCFBE6BC1A11E222A644B5DC6E0AFD7A329B96B0339C0D3E9744E4584FC32' +
    '088C1BED38E7669574410FBE48EB64D26ABAC9E8A0231AE2B7C26E21AF356B5E2AACFC665BCB415B' +
    'CA24CDF04EDD884CCE83A7EE81E704B3B709A0F8A49C8AB6084E6F0132C34432E5E2B92AD23143EA' +
    '6F93BB7675A7947514C1C5475107D7D7891C5FE2177339277E7F4C61E6B55725A1C3D486AB4345A5' +
    'F07FE6590DB49EED48456AF832F632633C69C7B15AEFBA393CD02D88DEB00336CC38213153C362DB' +
    '0001D950EA0D40BE697E6F4BF1200141329F1253A7421F01050C83F4579155C905881E0ED75299F3' +
    'F31DE83B66D9E2B1E3666E88D5C3C5624DCD011F921BF33CEDBC75988786074920E5E15B8648B238' +
    '4823275C61FA21D77F13F493B84F24B5FE4583AA226FD22A167FA6D7D441FD3BF48AC173BD90CBB8' +
    'CD09F4B4CB992DD4211A23D077A471416ECD4B3DF7501648D83E6E36B525E854CEEFB0F55422624C' +
    'A8722E355B7983B1B0643058FF5F591472CC50BB8239E2C979E8194068699D16D4A366851B62A16F' +
    'A535BDF18B2322C56F04B837537D31B0EC429AD522ED85C2F7EE2B77FE057A2FD7CDE6135FCE2E9B' +
    '3A112970800320CD1BA0B9610544218FE05F609515E7BE79371C6A9AE2DCA174E6F686E346FC8B7E' +
    '47120D6E87059E13121B936308A698A69C6736DA77E9A35B0135987FC3BAC2A61F6C2F76B542144C' +
    '2B9A62824BED766160E909FBE41C49A5ADB7F0CB279BE5A0E9FAE6FEE01723E99D97A4118FE635A6' +
    '3AC7BB22430C099649049CB9849F41157EFE7A21CF3CFD472A2A87F88528ED2E61B9071D4538B0A1' +
    '3029E876C53A3B9D86EC1BF22AE1BB3330B569D7A07556AA75DC66BF0AE810F933C8DCE8230ED619' +
    'C624F6FDD3FDB2E5FD61ED3210A65999DBF6A4E410A992762B6FB4C7DE224C2F0D38DDBFE133188F' +
    '65AB3365431C52B946B45DB82338EA21C18D767CDDBBDAA5DB42D0CFC635CBA549BCD02752D9391D' +
    '387854FFF33FEF60C9C09DB9636BCB29CC20FB63D3D0AEAA1D20459B7D45F9F6EB7F781613524338' +
    '6D801D6203BB8F3BDB942174FB5B663631C368A552FB22065258717128AA18B241702CBAAA8BF861' +
    'CB5AACCB635BEB8443FFDED4A4FF7798EC6F329CA4EFE9CACA136635E0259A8F63BB3DEDC569D6EC' +
    '7FB3238D8896A612F25C0BAE81B5D8239B1C2306299A5E1437A78AD607F478681793827A431D1B63' +
    'EA3C4B1D7A10A828A398C6FB00F4A122844D73A8F3515FBD1DE42ACA3DC2F591C9B5D6810A209199' +
    '96F05D965D8DBA7CCF3F23C40817319DBE6C8F052EC322D7127CB0CDF431AA4C83DB26F0C55FB2F5' +
    'D114D41AFC9C505DC791B70C1F90D3D3D5713EB0E5D3401C4AFBDB372DC9DC970779EDD7B15866C7' +
    '06BB9618FB3B013262A1B6AF92E1945E8218A1876EF5176B5A4C3DF922A22A26ADD8A7E670D62E1A' +
    'FF9CBDDEE4C2968E9E92B7CB18FBBF181BD921EEA5CD13610CE1FA048B54956C37BB268220778727' +
    '86683287ACBD5F7C1BDBDB5BD5F4379F9D9F80DF3519DB10E1BA9B2C300D4A271AFAB0FB0E58B78B' +
    '714BD2FCBB8F8C65FDED821B99EBF1F3DB3B1FFE117A615876930CAC9D65971BA566ABDFFEB5C16A' +
    '95F067EC55C2030960363908E61FA524241F315CE50C77F1AB42012DBF4016E0AA6CA756DF1E61CA' +
    'D3515C3FAC984AC24DEB4FF5F39CE2C6763F4DD89936F8EADEFD10E691B3F7F3F85668659495DCB4' +
    '0E011D58A7F6B656B6AD7E0F4909FCD20B14DB9F1FE2D9BA3C6BC9A2BBC000BF54D99EF2E690DEA4' +
    'D3C39D812E32FDE941BD1515884DFD0660C51F5B409CDE8A5E5D25A8C5D3277E14183BABEA0A7EE0' +
    'AC0DF1909A01D4D81551C96AFCB9C14C0C4F7F2903CADF5A884B2AA42D32281E92684E188DF44B14' +
    'EE7A67323CDEFC096A8B355EC24398829D19DE1AD46F3159781E10E67BB47900B0015847D5E546B8' +
    '8821D79E8C03516E2DC3D9DBB30E470AF05B652D92E5949A07933F88F579166FCC53143BF7979DD7' +
    '6293FCABE7A47C0184D2BABCA289C61D615F85B9EAD09EFD21A095359A2D5733A767340784C01015' +
    '5988DAABC5CEEF01CC00D9EB2D3A6AD3EAE199525FD99D7F2994F8B9C9FF16B5AE64F303AD62A76B' +
    '3D88ABC3E5B30D3C8ED3E04045F0B2B6A2FA75B3C63C220EE6CD639CA48B7A7CC3B41F2748947015' +
    '58EB4D76DE94FB2042B2E2B8BD29D049C4C5683455591F179FA620F6E1892B81C07B788F103B8097' +
    '88CDF98E758D17EE947D0AB391A2BD468F0128A0B146F51DC1967396ABBE0F75A56EDC0EB82EC3EB' +
    '41A80A186A052ED5FC689818D6B9078447143C0EA17AC979AD979B129CC6AC88470322C4A6526145' +
    'B49C4F4DA836D2188950E0C1F8DDDF9699F7A41A7F9DBD931FFBCF2048F015441463E2D0CB5F6556' +
    '8075C86E12898CFC47BD57A4C0C4E1D6D91FC7DE07BB8622F50A0CB4C3360171A91725CE01484616' +
    'FAD89D9322872445B38DF9DF4E0E889A84C6019E1CA44BBBD7A8843D4A5986C2ACC5843B0E8F430F' +
    'CBEBD0ECF1A2260EA268240538954230F483345CFEFF56A092F27FED5D55E01D0B722B9E173CB655' +
    '54F9FF76836BD836507C524F28E1A1607FD53D1672DED28DEA8740E0BA8D4345F9E31735C5A83D0D' +
    '62C6241D15C71858A27B47CB3BF3586CBF58FA572EE5043AF98A00649F9AFC35998E21DCE948ED88' +
    'F78F613FAC4BFE472E57C5FD9E1ED8784065D96DA5FE894669F75242C62A7280956A67E470536676' +
    '087EE35827C2BD328E1BA6D48B7DF6E9AF6BE8DC57207A2878FEA3905196A46084487FC2E21BDE51' +
    'C35612570F88654254C83B1218A12360EDE3D027592AE10E1C7A61335E7BA91386426807271D6B8B' +
    'E907A587C9A8CE844AFFE78BCFD1380165D4F88CEB3FB79250B1A186957D94BD723F0A1A03E325B4' +
    '0650921BA5708592D51E4EBC6106F03DE730BA0406F91471A82615AC1EE8208D452B85302C3FF68C' +
    'B3AACB32A2309DD82E4FBDEF22EF34024FC1B81EE75577C3039ACCB9EA119068D63C85FE9840B193' +
    '68406760D26115C0D3AD8A7CE80F06CCB144C46EFC6E54BEA8379290371A11B200E1015B18A170AB' +
    'F985FAF4B5F8EBF619EE5E0325B0C6893EC204D0E611A83F076F7850163AB2CA9532423DA6796447' +
    'D3334A5727D8D3CF140A620FDC77A110C269019BEFF5B734A1267EFF35C225731800BC14162FDAA5' +
    '4C5561BD6EB08D494A21F582CBAE972781E24470F268584CC00F228871C9486D4C7A1FBB7EC8997D' +
    '7EE79C37ACF9709B2FA3F6AA0C2AD5601ADCBE95B3256AD062DFB0E36E241A56F86BB470808B1645' +
    '1CCB5EEC185947D89B66C5585D41782E39A9BB12B4660F58570E3A1BF8BF017F6C6AE6025954FC9F' +
    '6AA4926985644CCF29FCDFF0D72E2E5576E1FEEF508720C2AB46B0F8AD1020A3297E7087C42E33B5' +
    '58CBF729BB2C7A77080FCBD04277D8BE6D82937797E9281B08BD6D84392F89D494E1A3B891CD57E3' +
    '4B9A3EF778DE45CC17366DE003423BE3';
  S_PK =
    '8C2D4B1CC2A8FE9AF52F21ADC92D487F90BA64BA71EE1D0F4466AB2505BFE23B8634DCD99940341C' +
    '6595013BC5C0597BCF796BFF36407EAC84E3D3C36FFC40A8E7F5EB233565F5B138A79CE40E60020B' +
    'E1810DC6297E00892B131E4E3C36E214392F976AC1F321F8994624250BDE060C232BCD5FA13348C5' +
    '6ADC67EE7DE869566AD038540944A8C111EE61A232296E9E74C941120A9AAED48A609D3FECBDE756' +
    '6D3B18BD0D189A6EB5439734D1CE96DC1A2A4D486CC934132038BE9F6633CE0C9B6F1CBC1B7546EF' +
    '9BD562144D05B38053138AD383183F8E4678F01E0FFA2A990EE0A4E3E64114BD3E769A1EB24AC27E' +
    '37042AFBEE6EE3BD9592AEA13A3233B07DBC869437B1676CAF10E42A3E50165D207579DFD683FC0B' +
    '2FE2FDB77891F669E6269AEDAB79D1CD8659FF48F309C97FBE0CBFB01F15E912F2C18557F6EF6FA3' +
    '5BFF6B557267A92B39C2A2017681E27FBDCDE640320EC144590CF9FD69FE6D90F28589CFC28E919B' +
    '6977438043AA1A492822944110B9BEF259CF3155D0AE6BA4D0CC8AAB8C925D9F5165C5D87763AA53' +
    '47D2446ACB6E07F3538A131B0CC6DCAD4BA2EC738BCACB26047E121944CD5BD0370E3C4E77F4E5E6' +
    '9D584E38EADD60838EAAFDDE062D1E9DCB87FADBDB20E4116951AF67CDC5C1705DCD44DCAB640373' +
    '41D389B37D2959A5A98375132CEA9F4AEE7BC6268E282215D52F05282D850DCB10FC66BE7D415990' +
    '874EEB592BA72497A8AE87D9D44A883AF293FF0CC043829D01814117A11A5A7D1236CFABE21B6615' +
    '664DB637DFB1319B844D278748A2254B663A4A4CD08E6FFB9BBFBD937D4D34823BD65CE5E1C78126' +
    '490F3A30C6B8E3528BBBE8D22DC5A570280222C9453AB419C1A18FB3D8E33306CB18E21AA29A711C' +
    '9E86BDBE53706D11FAD4ED20E34981633BBD13E872C6490A0C39AA27C0B259E20F4424F7DAA85F8C' +
    'E435AEC4B0E2503EDB3F32C8361CCF20E0B19144ACB16C6E03F5A96A9ADC7EA4BE400F4F17017C2B' +
    '0454BBA70CBFD4FFC606CF7E9EB7A4C08C82F5B4055937C9FDBD2763EAB57EA557079EF17D53199B' +
    '81299442DB46A0E5269A509C0F2BE6A14449D1AFDD65802B4132B94B15291DE500B317714AF7046B' +
    '1092C45E5D8526AB27BB29154D9F7752BF5AC404A95B26ECB0D9E456B43692EAA5E77FB402A6A310' +
    '780642309CEED421DFD233B542E72BE3BAE621301CEBC3169948040247A0026B611A7423A649723D' +
    'C2FAE517A8DBCDB1E1602C7FDA2F36057DCBD42635C6148D4EADF165B8FCC8EE985A26E6D3B485E2' +
    '8A36DC88C3BDCAF287F97E707FC76BF9CB8F5BA91BD27DB23E91F19C02A64E7C3E0D3DE300374BAA' +
    'ECC8C2EA5A763635F138F68BE370542C51171BB82A13DFD74DF69334FF3D6F92C72CE238B6DC0D31' +
    '22C6683FC4E4E93DAA6E25960BC5F2A10378616AF3ECE352F06C6E297B172E529FC0CE8FDC77C65C' +
    '61A7083BD14A6997E5C0BF21BF9D3166CFC2EB874F9953794A7122A51B62B11A45EDE008661B8BA7' +
    'CB0937D2313611D176D01F9A28E753EDFE0CA48FF6C6E76D6D8B3EB2B2AC55AE6DBB9BA861CF5D63' +
    '04C345D61019F5F29C86C9F5BE382CE94E33B40A81B513A91883CF870D9C3E37E87F2BD1299EB96D' +
    '9AEB5CFCCA6D4202710B1B81B091085A573A92EE82F8DDD5C2F6ACCA8DE864CB7D39CB6BC62147A6' +
    'C4339D9C1D8621753432FF05DFCF0A1004D4CB6B44E9917CFE2D1D902D1FE9570ED7D77160DDA2F8' +
    'E75399FF27A9616C2CB544A59462BBB02D01145603037FE8E76F5378E4FB6413B45F3F416127F38B' +
    '62E9C8BC6CA19C6BFA4415FA505611DFB767C6D3B6A82A77FE7A1ACF1E91C7067E47F4BE750168E4' +
    '89A0D9E183007B92318B2ED296CC99ABBB0FEA9823753537B7575DC1C74AC49CB3B53B02543A9742' +
    '0E6A6D9A6CAE0F8F6F18732B7F3012EC245B6E3642E31EAC8F139DBE7BF5135F9B9BF12F6B757B9D' +
    'BFC9BC60CF3B3E1FBE6722518A79C39A51881674D3BAC63B8A69D2F7AFCA8672FDE1DD9655D4841E' +
    'D944043A6EF5EA6B4FE9CE5D8840CF926B483E5C817B88FE61C845CD6CEE702AD16504D7760C0E7F' +
    'D6640D9BBD24635576DA4FF6327413A3F193F4D109E7888F58DED1A5C33E77620BF145D698FA1C6B' +
    '45DEE163661E356DE6E671155D1A779FE9DC7F4D7CF9E22BEEE7949E9881F6338B914F8565D0B271' +
    'D08512323C4FB45690F5D69F2E1D3EE306C65B1C0D367A1E8606641CCCEF629B00A1BA9953BC163A' +
    '4E8F174698FE56B25524ABA65B0E0A2F48E31C36380B4EE1F2F65A8C2368787230CF7C8D1EFBF470' +
    '8138921D2878F1A1B80E5D01731124F92EE5EE0840A2BBEF0D532CF4F3C03B37A5BBDEDA94C123A9' +
    '776D5EE728878CFA6DE0BFD30C40981015644ED1B56FDEE8453D219320C68DCD1CE9B9CB6FD144A3' +
    '6A6E55CC922A25633BDCB323143FA568603A2E555EB3973063595F22CC2BB40AA3017578E9DB2983' +
    '2AC83EF7EF4A7F4DC7F3671CB070718E733B82B93BEFD1DDE2EF479E79897E34720F92B7B4DCC176' +
    'FE8600FD91085D8393261176CFF838F2775E58DA4256E8D4C0C353CC79D54B72C09B9F1654DA1FFA' +
    'DC574FEF58412BF9509D836E3598FF92FB483269179E22171B5F6FFF55166DBDF31CBFBF4A723ED2' +
    '86537F6D51E8F8A02018D3BC76D7C2B3F5EC74F586D455A6B685AF9FCFE2539EB1740E7FF954814C' +
    'FD3D983C3F9C5DFDEDF3B86DF4AA3B079BAF2E34D554A722B78341ED404AA9010F1C2C5C189BEBD3' +
    '3E41D1A76E32B7AC8574FD47D6E5B63EFB088982C2533EFEA91644650021185A6E8DD64DA6CBC23A' +
    'B7CA34CE22EB5B375CEF50DEDE4C5AF9301D55EC3FC3979156B389EC84F697880F1CFABBF0A6610D' +
    'A20DF0091E430E742306BC349525CA502D380464FA00B743B6EB1F7D7B5185CB58040ECF875A2182' +
    '40FE910A47F9BBAC00F70CAA7E191F627707AD5DE74DA9CA32B24E6377100AD288540DF895B4BC21' +
    'DA112E020EAD3CADF1EE0D4E8B42E3DDDC119ECF9D8EC2C64E4F44E6650ECDE938D7E27707414FF1' +
    '659250ED3E445B2552E9886DB39FCA44B35D13746F54059625F7FB3A086576D89D1176C199BCB395' +
    '5C8FEDAEA2AD18EF856D59F000452085E4E17A130F890C720AAADD40C543CCA6F6F3A37734A4D16A' +
    '8C5E92F2D71E424981992FB333C7BA59DF6C6ED0A38B5EB540D0CB3A6650BD0ABAB23F9C7E038565' +
    '33A5D3E89DD8E580545D0A645997371B41424ECB40FEDA868B28B91C30E9965CE2EE176C01A6D1F6' +
    '09487C56FF2A0B371EBB291327DC4EC9286190ACB14FC31D5894E015CBBD8E7F853348E8DAC930E4' +
    '96BF7CD6C14B8393332673DF30DC90AA00C853E4D8FABAADAB6D96A55D80054722A693875D2D4BBE' +
    '8CDE135EA50E112B458C761B549DE147A659A7651CD37C8D4FC78AC7AB4642D36A4AD373A9053556' +
    'A26763790378E636487711BEA8AF05CF996FDA3BBB71AD88AEE719DBC1C9BEEA61551330D971C6D8' +
    '9C4A2062833F2B256B60653398F286249D0674A9F55B5559DA8A863ADACFDFFEC941F20FDF573361' +
    '84AFD9502DD72C86508773A70A445DB0896EF540E7C0F3D90B0654DF9131B8AFBC8867E4DA8979E5' +
    '46966B0EFDEFD40946DD32927594FF1A08D53B1B6C1CB882F5EDD1BEEC0D47A1';
var
  M: TCnMLDSA;
  Priv: TCnMLDSAPrivateKey;
  Pub: TCnMLDSAPublicKey;
  SK, PK: TBytes;
  S, P: string;
begin
  M := nil;
  Priv := nil;
  Pub := nil;

  try
    M := TCnMLDSA.Create(cmdt87);
    Priv := TCnMLDSAPrivateKey.Create;
    Pub := TCnMLDSAPublicKey.Create;

    // 测生成
    M.GenerateKeys(Priv, Pub, '260011F8FB1302750C8C5985EBA86998C4D06C8E2DD8EAFB392CE40D6627F000');

    SK := M.SavePrivateKeyToBytes(Priv);
    PK := M.SavePublicKeyToBytes(Pub);

    S := BytesToHex(SK);
    P := BytesToHex(PK);

    Result := (S = S_SK) and (P = S_PK);
    if not Result then Exit;

    M.LoadPrivateKeyFromBytes(Priv, SK);
    M.LoadPublicKeyFromBytes(Pub, PK);

    // 测加载保存
    SK := M.SavePrivateKeyToBytes(Priv);
    PK := M.SavePublicKeyToBytes(Pub);

    S := BytesToHex(SK);
    P := BytesToHex(PK);

    Result := (S = S_SK) and (P = S_PK);
  finally
    Pub.Free;
    Priv.Free;
    M.Free;
  end;
end;

function TestMLDSA87SignVerify: Boolean;
const
  S_CTX: AnsiString = 'CnPack';
  S_MSG = 'E5AF86E7A081E7AE97E6B395E5BA93436E5061636BE5BC80E58F91E7BB84';
  S_PK =
    'CE3163CA78FC9881E7E6A0236E5CF4050ABF431AB56F15819274121EA09448A375B9797A79C2285B' +
    '0BD5BDE4850AE39BBA329AEF0AEC762D5FF16F284E0A523C4C962617A356AB5BA09B92363ECADEBA' +
    'D26656E49177E38EFAC23DC6D16DFFA47120B31A17ECC1198F8D3792462B5088E6E33E91776376FD' +
    '61CE24FE5E687C8E34BFD7665F0B0165A00755222540868C74967E1737A92CC3AC8BAE4A931437E6' +
    '1AC1061C73B66192A91792DA33FEC9EDC7358E13642D91A779D1ACFC291AEB2A37A9B891AE752438' +
    '2E1F94A34A170B5902A4550DAB8D5BC78185A716D45399639652E2C1ECB52BB38DFD9AD983F74BEA' +
    'E10ABA3893CD9A28A8BD56CD8966FB9C7D7A31BCC0DBE24E499B3628138DE3D04CC89D35150EC4B9' +
    'DC09CADA134A8DEDA19F0158018F8094D8ABE9869891868CB8DDA128A889C1E168BED4D4ECBE0A4B' +
    '3CB95856DCADDC132FFCE781CAB0736E2BF4F8635927BBADD59022767C6A482240E766CC4177284D' +
    '061E8641006136A0F5D1252D0C73EA9509BD7877B52F0E8EDE70B3C45E6E03476B7D1D5B6816DF69' +
    '389581EE79105CA14DC397CBD2CF8B65C342F864100626AFFE1879830655488049EE55F3375E2606' +
    '4971472EFD3B048243192F3001665EA8A3986A53A449A3770B12499FC5C5DF4D0CF49FB85CD95B7A' +
    'B2CA3C505C1367DDAF1C8E7A2FBFA92AF16D1F325C4981C9685E0248177E517124FBCB0C43A86B4E' +
    '526179EB7E74459C450D94E1DEC2FD9D6407BD1CB72E23BF22F2C6D2D157E497076BCB36D5FA0967' +
    'B824598F0E09E6D8C7EF469AAC82EEAC9B58265B8E18773D7AC6F3F6D190FC1F9D32FF25EE9B166C' +
    'BA120AC2FE6EBE68A4966A8F98E54C67F22A6939BE43638359ACB8B9B94B62FD1114ABAEE86EE59B' +
    '241804A97985967841B6E392B8054C2E4261B4F3D44578CF7EF591D82F6177B601F08F8FC1A0DD32' +
    '4AFFB73BEF367F572CDFFEA93D7F3C44D525218E9DE38CFD4555332EBC5999A64AC05F08FA038E44' +
    'C328A47DBB99560EA6B7A421CA36E2C2D4CB771F86552EE4E8BFFBF2986EBFE24A9AA148C982BDC4' +
    '126405317D6B9E68849AAF73164AE96F7F7F31115BF738C2AC1EE600A9FCFA739D864CFC02E573C0' +
    'F5B30F95FE0398F2C79BE02AC106FA60F773C8ED0070F5ECCA6A89024D870A7C6A1E31DCDA5F62CE' +
    'B9074CFA2C3EDD4318819A2764FD0554AA1A7F741B6DC599CB822B498EFC5DA7B09EBCDD2E5F5BE0' +
    'F97296796C6054646961240F94404BD9C70A175CD985A38DF25BCB2041540B9B3CE5F6033D7FB13E' +
    '9417BBC2B1A2D690E3E518247413823A636EB2E6F8FB9FC19D5A8AD2C19D21BA02FACB2398D19983' +
    'B94D76F99A608C40E85FEF43DD0D2B99EE84F3B538945A00B6A1DD26C21E95AB796CF70150CF3C13' +
    'CC70A281D673F8FF60EE2AACC73C48178E14CAABE8312273E58922A2DA17E6E33E650C03EFB2EF79' +
    'C48063003F366FF08EA41CCFAF6B3610320A413F4C7B9F0B64FFFEC8281A1F4A4423DC683BFBF772' +
    'D7E5930358CC74E0FC94B01EA2FDEE9AB920D1462F9255B765472F2D77FD7D0754C9AB82673C9054' +
    'C324178C8F904FFECCF39B6770B21B8BCE4685E562E5804CDEE4307D8A21DA792E09010F0B3EA365' +
    '24CDDF60F1E192330594416A039D6AABA1CB55F3065FB15B54E48EF3D5050E3447AD204B4FE49CFC' +
    'AE3C9941DD261C563A018DFE214B4469D7646C07A90D68DFF0A8D79D724283C49E10E919D4922544' +
    '7A33B62357EB18F2EDF2E07F322FD8142518E77A358C63736DCEE6E8B905DB99297956E15CFE7B0B' +
    'FF7B60FED6F5FE297DD2051A85562ED71B7F74DC5B55FA47EE28422FADCA8FDB24DF164FA61E95D9' +
    '1A3A2C86563C16CAA22EAC374CF3000820ED1E857F96EA1F8605ACC153DEC64722BA66FB5E32C78E' +
    '160EB40FA3BC68655FBCED353B5AED75504C3236932760CDBC1F1E1428C3B884878DCC1BF6BBDED6' +
    'A26B249F2DB0F5160D7ACF8B0221E9EED3E9B5E222F20B72799C9D8E5A828905F83D9AE12E241AFA' +
    '0C58E917D2CDA51B33A442B1EE8A74D98EE99765CB4A0778FA1A3E1458C534335FF4094E6A519482' +
    'D66CF8DC7B5AF4B78D8020408EF299FB8CC581E4435D659AA41C242FE62D4EEA0B687CA92F269D65' +
    '7DCF92C1B6A5B9B5D86506252733911F2A7435FD22D3CC7E7E9D77EC26307F1D8FE54C98362CFFAB' +
    '13CF267539EA7F80C948A455438658E54421C4ABE580ED7124095B809324945B0CD58FFD1E6D76A7' +
    '98CB9C54F8C2D5B7BE3603AC6E3218947A045821F36CBF221DF2FB23E8193164BB43AE3B3298A8AE' +
    '35984063773D60636E3C514E82A216F6351AA40FFFCB6F4F8470E67BAB93FAAF539A82EF168167E7' +
    '3086B6EE95D9C57CDC63C2343A0AF30A936308F157C6200C1623C662608F4F08D5D2307F72852EB4' +
    '82F8534FBDD21234067AD59E48ABE5D451D7197DA11DA8F3310F7CF03D46C8AE6274B4EFAB9E8A05' +
    'C807E2BB4431B109E839059B348988DAC0953A9D29AF5EDBCED2BA526E855DEE5F5D621FA5C2A673' +
    'C0B8BA80A5D0E33BEF183B65174603A9C3FEF362ED9592270578E3504D118FD60E7C10C9176105A5' +
    'C1025099A171CC298A238CEEFC7B2DE14D8930C8D7E35697DFBFAF5C704D155E0FDFF094B4E2E79F' +
    'C3B7C9570A74718B8201FBB90C229A9C9B6E46798EDBCFA29BC9F94FED137C84BE2058873C731FC5' +
    'A78537C6A911359C407C12FCF7842A330BF5F304B1C3DE9FEC51C03BCDBEF2F129A1C9B236C73D7B' +
    '764D8D99B2167359F144C4F8DD4A029D0020D3E3C233F9510EDD33BBCDFC13B7E74A9EDD10687F36' +
    '0B06187572A0D93B822B2962298B0F05939977BE1B2919EAE561A29C24C6725D8ED7844340092A83' +
    'EFA2D1CF83C57B2E130C83EDB7F8814A01873D69862A425DD0527988C48A6507F1C9E6CCA099E542' +
    '952BBC5109056B3A0801FE45FBE66D66EE04A70C35CF4CC2FCFA4F5BECD38EDBF850AC1C24FFA19C' +
    '8C82949DE5BA8CE48D9B1EAC05FA8FC03EC82AB1609449355D00F4B9620092F6DC964B48678F3505' +
    'F13AA9D3156FA6C19E5410F3FB41E7B7470B140AD151E6787A0133D48E94DB3A0428593EFA245C47' +
    'B81AE3D283E66A7FA0B1EEED69709EBD847F752CF652A429366505C09501B99CB4B4B4FA051C2B55' +
    'AD4C7D4CDEB13907CCA679552A985E5573E7E9920282F32EE1BF4B89568804B167254DE1DDA9A375' +
    'E106806B22229ECA426EE6D546D4837921C6C3B4B654E3683638C4C89EBC1907577C0C59E127E3A6' +
    '14095F98EF927EDFADBACCEE0CCCF50A6945D08564E1C9EFB274E5788F4751DE0C60C7D206117BA7' +
    '26734004530D0098B1D772ACE606DA9454CC6FF02D7685EF7665553C256733B7016EF02262649BEC' +
    'F4A515FBC69D9754673799DB4C1FBBA4133190473B82650FB304CF582CA58983C4728D632F0F797A' +
    '900611E0BB4FDB65782E68C83A30AC6A42B0FBDDE14CA7E36345A44A4C5A6790E20623F3244FE640' +
    '9ED2482926A16CB655829391BED2748E3780813566CD8A942B3C0215D53484144CC58483FA854EDB' +
    '5524456608D1F87E2E081BA9EFACFC7FE31681EB1F883FF7876164F14CEAB38D7CD616670AAEA959' +
    'D2897BDCE5F2CB098F1A466638F1547850575FB9741A1C948262B67ED98EDEB8';
  S_SK =
    'CE3163CA78FC9881E7E6A0236E5CF4050ABF431AB56F15819274121EA09448A3B8BF58EFEDF3FD22' +
    'E37BEF0EBA8EE5FA9E85DF869F9ED4CB44525A58D2087C6923F0441A5387EA051398D0C4C3ABB682' +
    'B73F49D02809147C580AA18F02111F401EBAB0F7090BC6BC6BE5F4E0580EA5F01BAD159FA8AB04BA' +
    '73E5CD457118E167E4A26C2129119228719B3011242784043208DCB2508200021AB9485C18818286' +
    '21DB202448108558068021002620330042C02C8B2881230020A3B24D584452D040824C0284CAA889' +
    '4226110C313223B7249C3886D3186590C40C61C86C63A48D214070E4248C10C50C142611DA909124' +
    'A5291420919B28924C406A03324211B5100187108A34868AB829C20202D9C631890489632206E348' +
    '88C1106ACA166414364108284142966C24884819C240144428E3B2499BA221E3364908918C618831' +
    '83960DCB8269E32202D0080EDC404C58B2512127695A3482A44460141084939230D2906D48002C12' +
    'B08590B2250A82311A13102010110CB18051326288306AD9C840A14226D0040510B1210C082ACC02' +
    '8114C50D61486ADC04094906908AB22553848419312A5B802C0B226A62A20C11B068CBC651193445' +
    '90264810426689042114420292162CD0028C210804932031134005DC2864532024CC4471111824CC' +
    '9680C8140649349123246C12426DDA1468088441DA32420487001909658B960864848C41104A0B93' +
    '8CA4386190220600082181C0055A226E08318298A06DE0200140940409A230481400DB944920C885' +
    '621649D28465011400C4B8811144061335511BB0258BB268080251898248480465DB442E02162D02' +
    '1205CC10100035320115508C926988424409C168A0248608B37184342E51A2311B462422C78C5280' +
    '8193B02D9C0448818000A1262191B26DDB222C221846C812491B056261A469483282CA30701BA105' +
    'E3960CA2408AA282101C1271E2825103B229204891D1C431D9B08111A32C111281CA8031E1342E48' +
    '4629C4B221002740D844085A304C0C8251A2A49019146D40226C882029C4164D4B844044A0700480' +
    '101C958819216A10B9889AA48510482D83C850D4148108354E8B46511A2706143488D13425009788' +
    'D13801D98605480610E4346C88B24C60148A420625032902D22040C328321B2446E444491BC42411' +
    '058999304D00096210394209B04103C30512086922032E10416E8B22281B384D54868804034594B8' +
    '30819491D3302EE0124C13C7515A1432148669C2C0850C4480DB142009402104223023C37061B650' +
    'A112241189010B229049A40108258064C62C8C2042CC126023318C1CC63001075180268D04180C4C' +
    '962492988D89B8659A4410601406C9C42D90448623230012392222172E4AB4219016000BC02C0107' +
    '405C02815A944814252859882812C640D3B46544B42454B831C14260C3C2705440208202708A860D' +
    'A09431C8144020388A9AB29004908150C651842066894471D1288C240668DC863191C46D09C07083' +
    'C8488C022D22A829C9204913066D98104152B6291B0220032649A24811CB082A530460D24086C222' +
    '29449630539880C10662093286522641E2828D1047500CA29004961161102922B265184382943421' +
    '01208C4A102A1827410C26820A4145C9048E14A249CBB2045B948D188044DB348CCB220120271294' +
    '126891026892A4101A2705998491A4328199806012356D18042CE1824D52464CA4980918C029E026' +
    '2EE49668C09090234020A31006623889A4020651844122440503A98851424914C56D59C4294CA040' +
    '92B67101180E83B48C64980124093224916508260CD9322C92403100B04D50022CA28831CC206162' +
    '128894422924A4109A20289848841A32466088311344511A112894268C14C49100948509158D1917' +
    '328008010BC408D11881A3146C01444414A185A19844D918409A34260A218DA3064C88024684A80C' +
    '0BB4496020410BC94D59820DDA8441C3865109846D24240A03226A248825D00209C338648A482E24' +
    'B76051481204335213258A1C1862D9B01124272E63A2650A3372081921210661C0464462A2089146' +
    '2698222D6286518312040B404D9C384213B13108942148C62DC4226C94C0681299702381651B1930' +
    'E4022123C5508B1802D9A490DC426401C98108842842806192202400498D198684011171C28041C8' +
    '28285314249B829017DE42073C09B9B18C18EF46F3BE2CAD03844D3FFAE020B356D284D1E1C61285' +
    '396541289A8CCBC82E46C60A47DF8385DA75E5F8E46968AC584501C6591D7DE890BB9CFA5851229C' +
    'BD29BBABAAE87E0D5225678FD4B2A3C16E2A5AA1FCB9F675B2CE1008EF50B39A1914F2536E52B900' +
    '288CFD3F62ED38016D65DF8A9E276E634BCB336BDFAE706C724E4B7A056280C05E76DBA0B30C7FBC' +
    'B7E18603CC6D97F185529BA4F1429AA4150235C5F9AC7BCDB668DCA7EB2E671EF70A28B15FC0DF82' +
    'BA82F1391046A8D95AA9E4B6F860FDC0540C85DF0804F685E6D6BC084E34D2D618D97DE93F00377F' +
    '75066AA139CB624684DAD86E950138E88B17B218248F65437B1B3A9E4B0BC048AFCC5E1CF1CCF908' +
    '22F3B85E71CDE390A03CCAFE4AB1BFD1C4096207C50C16D270D8623D6A3DE7DABE1A218CC3E797A8' +
    '15D095C2300994F100024425559AA28FA1412D63F0DC7EBE6094191F662BA91992DBC60882781D04' +
    '4DDE5757BE23C9AE503AF561919A6C7BED92EA24FA4F1C285365D4C17535021DBD9A5B32CA1774CC' +
    'D82ED19CAF8E4B627B3446EDA8EDD6D91AD2984CEDBAE5DE6F11E86C40EA0A537359C3AC8A03AC88' +
    '618B2BE69B6101B8D8D43E6E9CA326F5DBD637C7ABE0D4FA9818115E6FBDFB2DE3FC0A6437EBF3A0' +
    '96281BAE5E41762D74BA7C6A5EB2E6D35B23CFAC823AA32246B5438BA6490ACCD97E1A3167F4D668' +
    '23D209921F9E55EB2F8F5B4AD00D777BBA30DED77046DB6170F3BCDA9C87234E773D6623701B1354' +
    '59747B85BD663AA2B9CA82DBEF589A8A3A3D9C68E0D95701C6C5F6B98D41C4AED0C42B4F15B1D53D' +
    '731399ED0D73FF88766B41A10BDC384A6EF56239C1BE90E9B09BA6D3AB6EE5A1741BDC94A12872BC' +
    '591E4798FDF175BCFEA72311FFDB9D56AB60FB5E5E21805DF74B9B526DDF84587B80751FE0061EC8' +
    'F35C1F5FD4753E6E26D2397A9620D966F0E76E8B6E92442CBD257594FC58A20447C05E361C7B50F9' +
    '28C69AF3E1E3CFE37F1926357CF20B6EF4C2468088F755883EA2D5A52C021A730E1BE4F494E274E9' +
    '20A6543E35E676947B3D5A1C9FB9666FD5ED8FC8883359DCA62FF0D2D51862D3635B8C6C9A23DBE2' +
    '2D03F8977893B61E2C10E0BB7512B9D2466BC13D8E3B32AAF66D670EC0F47D800332A12E0573FF57' +
    '0D2DC964999442284BEB1FD2D5C9315396D80DD20CBE6420E7C44F4FD9A14B7685ED73B6EF0B8AD5' +
    'D62654D9DD2C11E7F3D568B86B925227EF832D832D93661FB153142A1B6C6E0875088A70B62ED32D' +
    '13655D84130177EA445D69C2C354D679C1FAF09FB0C194D9684A4C09E19EED60027FE26F03106052' +
    '4DA5D4C8B7C8487DA11C424A20F1485543D4E12633E16C83D298D218F0F155C38381C9557715D6D0' +
    '7DF28A88CD0BC2771F5F70D54B0E16F0581F0017C3C6A9EBACADDB04551880AF01DFE7C10E44A980' +
    '894D510CB9F136F41816B578BD643CEBE6787F0CC65EA16987535EBD38F38DE984DB0096E0228D43' +
    'B8CB0C16EFF5E37BBF0DA88AEEAD2AE8CFE69C3F1A442978BA36BB9DA199E770ADB2EB0C245AFEB4' +
    '7E4F2A700C9E1CE14F824CE380060B9DB288439C03AF6CF8A9ED6A1363C46EF7987783FEBB4D6FE3' +
    '3617D4C3C300501AB23C2212CD8D41181356B603AB4612E76EF33E5DCBF88D36E93B081D2BB6F73E' +
    '3B6F35C2BF8CDEF23C8551109D8892FC6DE1CFC9E4FBAC47D031E3DD736DE631883B35AD59DE9712' +
    '0BB15872AD4F3FA9DA5B21A3385C8CD33E5AFA9ACCB2EB65BC1B4D05E8F5063FCA42AA17A0329313' +
    '06581B5194C2E68B088A7507EF7B6B3D4451373054483F73E2E1A9A8A8652F09725F12AE69280EB2' +
    'F7AF97490E6927A44A0BBA80A1D4A4D7301ADEE33E84BEDD083FB33EC83121D18EA8BC577CC9BFD2' +
    'EF68BE5E70CEE8E65B7366433E9CD10C6A9062EC655060118539C40880EC419A74D406EB0E6920BE' +
    'BDB868C9FB18D0AE3C04DF873CFD153E163027E89D0D32353623C604F5958407D8A5294E16C3A5CF' +
    '80048D508E3D972E7037362B2CD7A0FEC47CC1AB90B68C751A474C124BF32DCCF52889E7CAF9946D' +
    '0BF1E90B934AC9EB8C45397961569E0B6BDF11E1B06FEB01F35F690E9E9EC062F7F4E1B6DD5A8E43' +
    'A5712379077A7620FE4807F0E41A35B4A339879E8EA8C1F382D1AF2044960ADEFB37D7ABFD2F874B' +
    '18D5A137D08BECC6B064619CE171D82C4A5AAB399D7D45BFEB3A9468591533B2A4A0D8D91447F829' +
    '49A3C5C0C071C4D262EA7C943ADDF1D2B4A06CD75278725B2FFCB7DBD7B898665E8D9C4D42847178' +
    '94B64A4758FCBDABE3377E4E564E091FB0CF22F4AD099A614D33581C427C8D1ACAD031BF86B2D7B2' +
    '7F992E271384ECD493299A43F212428D7487C6DE03A4105B7F4D66C6F997C180F4EE3250B3CEFACE' +
    '487EAF56BAC2868786A005D9937584DF3DBA0AF81D308D149ECAAB49F39D0A15933C15D0E4CD1587' +
    'F817DEE0545468406A6892FA3E8FA380D2D288102386ABFD95B21B19A41673E2808F02DC90CC7728' +
    'A0B268423B2E5DA3325C6271F3666A0D28CABD8C69C9132D4B776151E69CD964D1E5F59201D5A792' +
    '98305FC37E24A8A5DA9676526789249EEF0C10BF669135488D13B3415F07C1E54BD275989DA6539B' +
    '9A23C2AAFD2DDFBA7116CD98EF6018DD609E8F9020DA976C10D5F9ABEB2E716B140AA5678977FB85' +
    '933C023E33E8A397ACECD87E42428BBD7505A0792CE0FB20BE5F52362A9CCB1B615D663C43B09421' +
    '4139ADB669E2E1A06362AFB1E288331ADD58D4723D4E375F9097F1958022EC9DB8949300383A39E0' +
    'A58A200D2EBBD05338C1B6971509B716864DE1754FDA727FA09C52FF778AF032AF8900CE3E028891' +
    '39985EF5976D25497D2C61DD41A6299E459B176A83628C0D1CCC1B64F8BA517835727A51A168105B' +
    'A302B92726A92E8676AEDF87A43EF90A3AC4478F3BD88EB3270374853DA5A685FD141221B757339E' +
    'B26F5345294B6D155FC15C51F41B3D538029EF089FA6F30B4F9E5B6FE31D314B9076BFB5DEDD012A' +
    'ECC9F0608DE4A67FB95AA43F8813E23B610D20E2F9BBF7FB276B9B4A6678B639C743F36BEF0C5D8F' +
    '2F3374F99C06072478BBC64EE8B8FF4732859CBEA00FEC1C73799F068DC68D43CDBDB538B206DFCC' +
    'D588F378555A019745B839E2357FA16CFB46D344B22B1951F410FF075133F17E6701B5ECB3ADD5CD' +
    'B0E279167B899C0D6E77BF37861DDB802D55EADD8CA53281FEAC0C29FC6BA29B6655C48101CE9A2C' +
    'A5B5D119EEA5F2738B30CEC24D148FF4BEB151FD548D6415FD372B3D056F59A0203134D855BA8A66' +
    '8D8E846AAAB4264A50298CED797DC55CD0E9522B0350F73AD5BD51762BD4DC1A8D4331F609B284BA' +
    'CDFBEB3E0F782CA8B8C720CED9B753BD688DC038E163B374748506E1785AD022B6683F1CABEB4354' +
    '555577F62BAD8138189069DF8A7B44F711A4174368F29D3AB16D50E703B91B76FBFA426E123598C3' +
    'C8434BCD0B290CC3709BB39DBD3EB53A8627372BCA429B0D4BE3F6C5A4D8162F23261EEF4BE89407' +
    'AA8FE52A44121D07B6A85171E40DA9AC7FA137ED6C482C09E0BEE156AF3DACF8BB818E5F905D9B17' +
    '40B112A0C595D24705609F201AB4BC1D24829EF27A802613AC5043FF15A2A7EB9757E1DAC343BFC0' +
    'BC62030431728768B2409034C9D91FD12CAFED306D4705DB2C47225B5268714EE5B1AAB21872C245' +
    '05C271D33AA7F0851C5CD9368213A59A29790D909676E48A3F0595792538A4348BBA83363C6A6553' +
    '37AEF54003F6D93072A392A311D9EE88E394755A023C87ACBD89CC57D43B65E1F62A20330E2AFC62' +
    'AB476F96CBC385669ABA45C39AABCA07791022F17532B8FE5BDB5B157F3741B775579023648180CE' +
    '691E217B578CB22D25E843D0DA4A097399AD5BB86E11655B7E5A0BE70EFE5E860C3450AC9529BF64' +
    '011275B2A7A267B2930A346AF2C1A9D15C2C2F72A3E57DA0C502137036F0140FB54BF3C39E1D153F' +
    '84A2115C210A4A2FA658DA32BAE3A484E82245DF7DD5C79E37BCB5256A6576380E3D803697F669A3' +
    '3279CE39D07E4AAA3469AF205E09D1F16D7DF41C700A5072DDE666667CB720F91FE0A9309CCC4388' +
    '1A7AE8141396EBB0574C1E28AF9D1388D79240D92051E0ABFA15481A6CE86B82BECB1A0AFB9B6822' +
    '4AB1CC65C8FE5D44063285249F72504DF3CC02E4481236CDB33761F305D67853C1033D7A9B772383' +
    '3F360FB320B3D2F28B2A3512ABAB88DF0953A4C67767930A857EEB4E90722D4CAF0B7D734D522CD2' +
    '691255B6458DE86C8D201E317F4B937BEBCBE9942EF178365206D91CE9FB86F9E84CC8507EECE21A' +
    '07D55814627C0AEF7B69BC194DF81AA058A74586D163DC5BFB2903F0D74396D723D7D369881A7CAD' +
    '1306F24F771AACD7297452F2691B6ADDF4E9C795B968137F279DD6988948359415FEA44A6668A987' +
    'AB48E0F3B53C327CA009FDB968B16CF8606979A8380057A12536ED458E217D37C3F4CB52F3297C00' +
    '50FECA831C97A50C5C3A2E66E46327E37F27C8060CB0DE6FC1177BBBAE663414E1EBF2960E319911' +
    'F8C1FD2E3CD1CFE9972D3306097F10A1EF6AF226B8EB7F093B1F6E2239E1833C588A03808D2E704F' +
    '730268771F01F7FF8F5174FD89438B9F0878332B523AF2C88CC84D702D9B484B2561EA0BD60DFB8F' +
    'A45ADA8025401C0C7B7B7AE96D5DB510';
  S_CTX_T = 'CA2AAD52AB8AF12CC0B9D63DA31BB978A9464855C1E0D281C6F917B5A8723DD878';
  S_MSG_T =
    'FFF6591354123A648016BE8EAABE83362E82862BED61138788BEA16646D619BF841F1C16390B1125' +
    'F3E8356C8D76B6E328FBEE06BE9CBA1C0D921E66372E4CB5834C0461A2AB321D8844C7A77D211AA7' +
    '24D995928B9C0C8980BEC26B289969BB8FB28DA81DDBBCBD8E7AA2CA263BDBD0FE47FFA1EADC6811' +
    'F5223C5F68C0982A0FEDE258F118EE247B7F6EF4AD01ABBD1E2B2BC0C479A9720B7FC00BFF383DFD' +
    '6F50CCD1F8F9FB2908C31A944E7490B52D3B97A6BD087BDBBFDB51EC42F6E79514BD693DB7D9F9B5' +
    'D1E8C93F54694D5C6B0201E7CE51BAD88D84ABF1AECC12D1036FFD9CD1FBB82F2265F877E0085F6A' +
    'CD00B954C7EA0512593A3970081C1BC960A948B4D41A5D2457C9D1D3641313C7F4B40DCA96737B4B' +
    '23D2B62E2010F8F4385C8BF3CF541FDBCA4FC8423628E40F68598001105426EC9D1808F3F528741F' +
    '0B08FC49237B2075B61E1EBD6018632BC6AC56943004FC35A5AF6FF0C9D5F9E80C285B9954ABBCBF' +
    'EEB0060D89CC9C0D08D1C8EF10814766A151F4E03B0E26BC2CA789FC266CFD8E9B6716786B9E8699' +
    '8FABFFEF91269096258206523A5727042971AC10662CB908C15A16A1176CE9A8A32E3763134F2DD9' +
    'B7EF29E074BEE9B1092DE554462AC8C70E123EBF0048854E627A11230930310EC6F33A2EF4C81B00' +
    '1916E99DBB4B22B4514193445DF4CDF21D1DA88FBE02C8F2CBA0A069DE1908A2579CB3CD3AA36B0B' +
    '0ACF26D123FCAE665D1948FC6A729CCDE3244126572D847712B7410B5A2A0B661913025BCFAB170C' +
    'C6F79763911B5999A70EF8450A851F0D2AF67D3A08755EF57CDA17C40B44DBDC6A4BAB763AAF6D8F' +
    'F24A80B820874031D80E9FAB888157726ED212FFAB4BE428C718CD4AB7F71BCAA105BC60E7CCC78E' +
    'D9C310EF6CA2F43795636D56790F95AEAA6885E71619F4E8FE0C4D7DB8ECD5DB288B0E40FA8E914C' +
    '00E364ACDF19D6EE82754CBDA8F01AE8030A7EFDEFF51AF90F4D704FFE1823F7262DD83002F8F27C' +
    '093C72D3E6E52899DB0C581D86B77AD81466A4D296DF2BF54833A3D189F6F45B2705B469CD2F60B4' +
    '1F062F290D6C2AC9FF6127F6541A81BEA63F03469948A00A366589B180793A3B5DF8EC189ED1344B' +
    '7A3C1929002BC1B1B635E3076ABDAD0B1FCDFB42B7A406198B272115CB38F38A476D044654BFD831' +
    'A741558693C5637F0DB850925B7CB97E836DE9D95C68F20CBB76CDCE3CBBC4706057BE71238A8674' +
    '017E7DBE44AF00153C4002182F398866B27910193A64FD8B7694415C92F5BF85861912E5CEA585CC' +
    '9007323A463A05A2B35167E2100D8D14634C63C97B911D6E7B485235DB3CB71A5B2027C87861D8D9' +
    '5EBDEEB0643ACED84EEE2186BFE0ECB7D0AB9B1E13635FA101CC1DDEE5DC0285D223A96849ED1A17' +
    '6766983BDEB9E678FF0CA47AE6AAFC0826379D7E51363EE729003FA559ECC4742682C9106B85D99F' +
    '4A6E1524C32B8512C803550D624099317604C1B75831E7143067A66BBDC50B7B4400BFEA67B5EBEA' +
    'B798D64001F5DCCCED87D577011E9F0A597CD9AF153CAB9F4356023E0C87B84C7BC2D5F83B40910A' +
    '17BCA868654ED1473FCB72200167AC8D90128192FFF48896F03BC5D266DED6CB9CDB17C2CA3BB2C4' +
    'E90232941DA88741943CD6110C2EE52E7E73A41713BA4A726090C889A941C967238DC0E522060995' +
    '52D88DB150375C6564C10393A3D552AAE8559B273EDA1A22ED274868934AEF820748ED656433C7FC' +
    'B70664C463B68F6CF65430128810DD403EF2E41833136CC885674A324149FE196824381B1A5915D1' +
    '407C7C001375E9517B2FAE634D3021F0227E1D7888CFE8735444887262586FED55314954048E47A3' +
    'E0D2022ABC525EF1979B7489466A0389D0808E5B18CA247D596BB6AB21B88B960863E38B59B9F4E3' +
    '9FA13C87D5C8580CDD6A0D8F254F06C45E5BE3B136FE92D4224A529E77E0F4DD10B4AB1F8E83D83C' +
    '1DECA9B38CADE05A025A6C4D9605974BC0CB0AAE0575F8861E993D06AA091FCBA87AB6CD49539F2D' +
    'F858146F6713774EBA5528AE4F81B6D4B9F2072CDBD9B317CAD4600E5351A0E9795A2FBC4AB27925' +
    'C518DF5D20E99F09880A1FE93C198728145E21E0393D6799B34997EF8A3FDED9DC44456201C776EC' +
    'B4994BBAEB54321149BAFB288B55690B7BE12596D654A2D3C50B3E6F2BD0E31087AD36A248E27BD9' +
    'DF6E432D13A3E51AB37133E5D8FD9AEA3C949D672D127F8CE70FCB70701D7CAEC6FE93703FDF6762' +
    '726C82AA5DCA1F531EA6C0C59141D773DE820C7A065786361C90AFF850B891FD592EA905865D0FF4' +
    '8AD1C8F654983E76F6E428D0DCC1911B7109086CBFA924E79D155ED0B8CE570753E93C9773F83F24' +
    'FC6EA2759F2E12B7FE61D4DE6462DDC6693CEF94BD3BE59959C8FEEF79907D45E76DCE9C708E238C' +
    'A91B0B9D67B1CF51D8B528F03E2692CB0B1203C3BBBCD30AFE3C8F34B4B57B912C2DBA5006F0AB5D' +
    '8CB9AB4C9A7B8276A28251EB1F4FBA646B118E130E5B0B8302C26CC09D354E5CFE231375203F1B8C' +
    '4F9D90C0397E5398ED5885544DA9809F2FCC8CB508D11433438A6A73E8A8BDF165DBBD8EFB3B4F8E' +
    '787591B26891E76896FB1DDD3080680C2C8BFB880DCC0E7F0AEDE53512179B4DA9BD3671DE41BF3E' +
    '137740F627C8DE4A11CFA59688F94CAFE66FF327CFFBDFD6F245A50FCB79DBD2228482597BF4EFFE' +
    '15B530F1ED8D924FCD02EBB913E1C295D0BBEB17C6F4FF41C093241B4819B2D12907089291B092DE' +
    '5D6C04CBBB9C66469FCB732DC619061D47C2A1806CEFBBD1CEA6567927626143E9B130CD12470362' +
    '14900B04EA8078808130F4F2F37C908D391613BFFF7AD6ABD05EAE3E99874E3BB495FDFC7DF5E7E1' +
    '6656F4FE0D31A198F0CA546083B8F8118D89FAD262DED6752E0123951581578E769677F78C6334CA' +
    '30111D70FD7C3DD82A127DD5835E78DE4D806141FBD2A90E3A51DCC19022240A5E7E30DABC24F590' +
    '431E67BE8F92E05357081AB06E924861EBDA94F30C81F792D10D6763B1D0E66830D0D3443395E8CF' +
    '44E94287B5F517C5E2E2ABA38C266017639A547761253D9A90CFA686572A43977D21196D985A2D26' +
    '85C888EF6B7638D7562B6E762C65F05E57DBE1E21C9C8E6AD1161AC5FDB743D11181DD5183947671' +
    'B2C223E59515AD97E61725DC2B3FA7621A0C83E1E8EA997A11048CD9FDD7757CB70D21C3FFA87755' +
    'F802AF973155D69D5ADA29F9112E7FD485238E0D6AA59996AE563101DD023D4305951067BE1F9DA0' +
    '11F67F966D24D3E90FFEC53DEC7DEA086A6EB7CEF5E794DA71961EE4178ED24B607A8A54AD030466' +
    '2526E971DCAFDA247928F6916BFD1A9B4A2D9A728BF90FF86FA1ECC6A1643060FF22CC1083DF070C' +
    '08A9DD278742ED4C402CB8DE640488F2366C50E3BCE3CD574F7DD4584A5BC8030BFF7613EB4665C0' +
    '85A8A96FBB80DDD37A176C4BE2120B7FBDC96DEC0272F5EA9B745F41D80309A2B832F6C7628B20BD' +
    '69D6CB3F4664786A404E17F4E91DF27C700BFADC3E46AA4656B2B05A7D4CCDD4BE570AF5EE65F696' +
    '519D8882496055688D74142F5FDE1473CCBAA53CFBC907C8D060553E8680F0843B5ABB52BA7600FC' +
    'EEAB2385159E3413959076DFEA31F10CEF42F9495D4413B1FA29CE94F1A9044299906EECD5F70BC9' +
    'AAB2829931EAAE09EEADFCBEB89A8D5D97F97D2168DFC5172AA7CA993BE0BAEC01B2E4EAA1ABAB77' +
    '4A3673B7C12FEA6329A9CC8FE30D7B8E9C2444DD61F9406264BEEEA3FEB6076389602EE46D613C6F' +
    '842B927B421EAAEF55E2A1A80E34D8791AE119FD9E6F8FBB250FAE8EFDD1703DCC531139725B447E' +
    'BC8C128D722D7238EF2CB5DE9F08C3966504D7879D667C677013D5816CC7A5D0C0B9066205F5C0FC' +
    '964BF57A38E051A2D57E01182B0537C3EE178333290984E4A6EDBFF1AB9815671D028CD4296B7FF3' +
    '47FF8BF0AB4BBE80ABB2FA1A5DDDFF23201FE5B7D498A2606FAD64E985E275E932E011292DA4038B' +
    'A5948A4EF9841481EB12B69F55B1D45828B895AFD763143500F49F715FEB35D7BE3A69A9F1CB1904' +
    '5C3B4C991875DED2527014637BC1C60E3D7C19138971BD38ABCF14338427C83D1C10719D7251B119' +
    '0729E31C24E1A6D36DEDD0BA083615CED9BFEBCA8FE5FB368C3845219533FC0E3622499E3F96F897' +
    '99A418805D5575613106D16F60E83B51C823B31C783F77866E2E0F7E9746D4AB2E4BB2CB37C45940' +
    '54BE600CBA6B3D46A0E1E5144226E34B6F5044B796CA1F92138CDA9D2EDC32AE38F6435C8AE7D6B2' +
    'E5A549B6D9E83809E9E0A869729136981AEBF4911D22ECEA98BEFC7A30CC2B18BAB649CC62397D07' +
    '1DF8C01AC7CA6C35707328B6FF90692F72ACF056EF5601A9FA6F34DEC7A70B43A1F32287525E9688' +
    '15ECD131762687EE9510746E7C1EFB407F12795D53812141A59FDD877FFF3FE61A6D75EB9681BC73' +
    'CC8760E8E9B0E27E7AA1869245B0F68AD767B3339A5475F6F95D1F5F9D76E106A4BE0E490CF9A958' +
    'D277DBF4F5394439CD136177886132DBD06CE29209B5F656967CCB89EAC39D39DF3B94CC2F4663DB' +
    'F0281594CC77827047F90CA932889367440B3A59143461E2A72EEF0AD179D4DD16151B9D1D3B61D0' +
    'D5DFBD4CA54568CE466646042379F00BC532D02E48584F0545B2EE9A57DACE8B105ED20540271478' +
    '5D3862217F1A8F04B07377EBC7722FA8E5FE73B9BDC507A988C61EB37EA17F3C1E97A078003503B8' +
    '6CAB605E8356512E1151A6BF643210FEB0635783D2FBB010D1FA4219A041F98CBDD7ECCDC4B70CDF' +
    '49F1AE2EB2503760B141B9F9C2C190587924168F369B20C08058109C92C1406A5D194563542935E7' +
    '8A78B5C10279E5CD737F8F042169F88F550258E738C2D86602BC73A78F04D7082837F92F2B75C3D9' +
    '40485E6B284226D0C0D4682B325E1F83A7E036C6532516D2EBB747473AB964BE2E8B455746804739' +
    '82C6CDA6D7C2C06F7E33E88F33F26486A0A4EC6F7CA8978E7B3FEEF2CE9EB7906F6AB8B7ABB776D7' +
    '26C3F0D147DFFD1D7E13F6FD10B6A43B7E795D4D9AE7336F05306E310FE39996A1FD2620E31607D8' +
    '8D88D9010DBCC1DE9CC89B454E18D44DE4D3E84FB88BA5581503B9D195400319BFAB666F341742ED' +
    '8026BCB53BAF9A5BCFBB60E730C684F3F70E1950A94104587DD6F5E873A40844BB18163C7E9D662F' +
    '536313999881F0B493D9133D5CFE0596633B52A1DEEDBEE255F968207B18CD0A4B764EAFD76794D0' +
    '78D4D531865E3E233891554CA8B1A5A212EB012D516888690B999B9A223E28B887FA53848FD0CD57' +
    '26BA880482D8A4E61070ED00201CCBCB58650254B564DEC195D7254C98B61200EB21491C33E1CE0E' +
    'ED50FF0000BE8FAD47EB3AA2D1B97870D6BCE550E792371070EEBEAFC912B69207D40D2ED4B47D7A' +
    '270CDFCFC683EEAAD1122FE973D6E2FB7FD4B57D39B7B926BA0025C063498206A65826C98DB6E425' +
    '6BF766823971932D94707857BB23D34ED66C86AAA95AA13795685B0603766BF72B70A10C068F9341' +
    'ABCF1A0581186E015F2A40F9AD10960D046C12CCB23EFCAE75F96F29253379166533BA3F67A6A4A7' +
    '340D440025E1F09B9AC7407FA6F874650F82AB25BFA30FA42B66DA9EAB30D445ECB8C427A40C3021' +
    '1F2FCD34D7A47C3FD2666F3CEB9060F935E80C560210CB67FC1724233847CC7FBE97B5209D458D37' +
    'BCDCAE7798D05C10DBFD6E708BEFB9D54F6B7A64DC391C81DE070D48A03CF7A4599846BBE51AD376' +
    '7C005C54C8AA87CCE0565C94E9E7D0029893916B46D9113851371B12BB992BAB00634BAC3BB41570' +
    '735421B17448DC75AA025510C6D3D9C8AD9809DD1AFA200208D0DDC55C7A04EDA4583F20A1BED05B' +
    '15360DDC9B337CDE29A82740A2B9C8B274C37045A4F1ACB7DEF14443B10271D5D9AB97E3AD94C578' +
    '1681B70932E3DFE89CF1231294AA06AC1ABA7160AB925FE0B25A2481A164A398D504EFAB9EC7724F' +
    '52EFD611E3232FFD2D3948512C9A7C052C55D0A53231D872020735C3A734E6620B9DFB28988C3682' +
    'FF56B27AD7A69F0BC896DF84297DED4D186E9E1AA62F54E0B31FF7CBA6449C0CDAFA22207FF0E0A1' +
    '21DCF3FB1499820369A16D3499117D2277326AB748C0E7BB92F41EAC8057EF85A2372B2351CB121E' +
    'D7408C8E002E00F8A7A3776E7583D94D0045C727B75943F62F6D639A0924C6EEA1E03B220DE7005A' +
    '1CD606C44D1FF205D2354818BAB3FE12629A7601881DCEAD67C58E6889AB82485D81DBB66CA590A5' +
    '6EEBB7D8D12DB32F49C4CD90305EDBF78C7269DEC561F1890E1A456DE348D7302AA65F64145B1FD0' +
    'A354CA7B359162A3EFBEFE43D5BB4CCD50CB34F1FE5784134003219301CAB58BB63E99122F425542' +
    '0B44C4675B17334B39602FC9E7D60F3C66FABD8AD36EE7B4255A5A0806862E7F951BC70E93EA0DFD' +
    '8DF501E97EDBE61147A0985EC1EA5ED35358526914C1403B49AEA5CDE756DCFE587F7532423A0C78' +
    'C46FCA47C1782D3BE9AF6570E9657C032744248B54AA8011E759D6EF254783B8C83CCF91C4687F6D' +
    '39F9786768C4582E6E9B7AC5F3D5B494018EB041E667346117CD344294EB9EA20A1761083F81A098' +
    '9ABC565AA89A86FD7E637C630A89859A02B7E9085CF3EAC5F5236079D333F87EF1BBCB6B02371801' +
    'D691357EB094C56A90343B3700925366275891C09B5C8FB4E22DF7D0F415CF3DEEE99AB059121D87' +
    'E3FBD562E7DAABE98BBF81D17506EA71418F91CE1BA96666962312AB7BDF25A82A06C786CDD04473' +
    'FB12795A7C66610095168F7FBB488EAB82A3332B4EA6FB0419FD37D6068087D00E4C16A954E809CA' +
    '4F605276E33FA661DDCE0655A153C012BCCD428630B061E78C73BE61A96B7988F8D47F0FB9B7D25C' +
    '83678768671443CDAEC015CD70029ECE6F0B2EB69B29C964366E69E5206ED2D19AB388CF7FD1F3BB' +
    '29598B73CFAFF762B99F7D8B896D0F0C3606395AD06D273933E47C6865586DED96B823AA35006A6F' +
    'C93A5247A705B6171BED27EDBE89734C04C1733A1EC09C0BE725913F842AFA5EF3F7E444BA2BFAA7' +
    '1F9A290164551228F7A8D5305F5E66AE7E0C73A2ADD5B244EF8B42D0B16BF5D9F1D298856F1A175A' +
    'C73E15376FE9EA35C896B0AF05EC7AB820981C7BAFD9660DDF451949942F442EBB788E278ED2DD0C' +
    '07F1AA1879D51E0B1C2CB4B449D3D25665A30A4CF5F12773AE298424F796E5D44C654F8130CCB1E9' +
    '910F3ABBA2F358409C86BD73A729725673DBE42ABEB01A57E856A7A1136EDC2316B17668489B4E93' +
    '5443D411EDCD2BF40762F00CEFC79B08778356F087AC837198AA40D21E79B864E7D4F25AB992DA3F' +
    '472E4E96E074CD578494E101FF3567469D8EA4C70C722255F9D081BD1F1F9D59845626EEF321FF3C' +
    '724D4541CFB2356991127957EB5E956D1D41B7DFB13FA4C9D5B1B3349FF95816E58F26A1DF4096B1' +
    'B032FDC4E8730A378AC5E74C37E5CC98EF24816663E59C8FD661CAF6961F33058DEFC57DD894EE57' +
    '1716193B882F78B086BCC1124524A075AFD558D6CB224EA401707BB0955255B137C68A045A4B642E' +
    '389518E4C4347509A05ACF2D45251EA21A7487ED5DB1469EA18647BB2D41D07F039DFCE9C4691988' +
    '5DD961DCF1F6377E82B8452BCB67CB357FDD4EDDBDD3E20137C1247CA4A05A609F535529E6C57BEF' +
    '63048E0F4207402719A18A1FC9B9DBB20248A16DE69A8310C888320C8FE5D609FC17969886E449D8' +
    '2E0A1628B123E8D25FE40FBDCE7807FA9D14F48CE06F3130C93B42B28B3B97120570E5EE942025BD' +
    '15127CC116A0443FCA22D41DF74F4914F9AB48D6E0FEE5DBA5255D13B7D11C7132EBDC29842BDC12' +
    '1223B070163C3762849DC0B09157A9F13C00ECF2B257627A92EA5A2EB2CDB2B95CDA2E89EF21DB91' +
    '18106A1A4CAE5C8C1A8EF736E439EC41AC9F92F2CE5072EEB70C5FEEE4C37F3436D02C9F980F1A24' +
    'DD297E24BC6AC6BCC8134D6F55126244AF022967D339B599B3B4A0ABF5B554FA0FE804319422F1E1' +
    '7332279A5C32E9D77B2154EC09290CE15A1347AE4B3AEFF0ADAAA13FE9DE6504EF3290E01B10ACFA' +
    '63ECE5E4A502A4B127EE10FA26E114041342AB092C4CC36106FAEFADDA34D06A0D6D905F3E023258' +
    'B3CE8BAD3F6CFEDFE06B14EB7BBA67F34B24E7D75B577FB29C4F6625C0F470272D26C4943F0F85ED' +
    'D24A055AE66CBA74925054AF2A607F7762CBBCC559D12AE5C5FA57352FFA44A0D31138DACFE27D58' +
    '15C0F6EADA3622855BF38E84AF59FE0A25D43D64C5D034883B2FE54CE6DB6BF14F6C1E58DB4FBA03' +
    '5F1D0663837FCFB85D1EB53BD4CDC53418D13A953982A3615711047F115C28DB0F11328A044D5A75' +
    'C41ECE893DC20353EC30B4042BCA1491A70BB1DEF594D62CA7F2FDD721B165CDD6655F4D134A6D08' +
    'F0F913E888D0CE0DE9533B6A1A31DA246BE2D3AACBF34DFF193DD4F9D8FEE5E375CB0FD0B8420E7B' +
    '9CC9CB6EB17E0AAA5E384BF92EE2D3FFC9CB2B974B6BD7B5C2EDE924DC395CB7DB028C9FF317CB44' +
    'D41F64AFB3A8D05EEFEB0914FAEA7D2968BF867CC3DECAC3E7FFA1FE41C3EBD3E02581C8943F7449' +
    '69AC03A6FC44B67D01CD63DEEBAB6956E8F9EF27F13A0BF3920658B0687EF7CB9D71282D35B2BCD6' +
    '2793394CB77DDA39D6EA468B0B12BA91DA3958B81BDD113A1A1A5BC5E540412E85FA3AACCCE52CEA' +
    'BF748E7AB811E6F9370D6D76E141D1613F0532E36899AD6464FF9DF10BF19F47AF3AFAEC3F513F4C' +
    'FEC35F92A5A52E22EB469E6B2DDC25B960CECBF85EE4BD96A0121C0F64BCA7DD705FC08D530E332F' +
    'F7FBD5C9883F5ACC495EA9E72EB0ACBE51B2A0D8D5251EC10F26AB87A86E180AB5FAA80AA20E4940' +
    'EAF234827DB7041BE668FC6B18E8299F0577D8D28045BE7EE0279E33CC93264D1EB9C0FFC42438E8' +
    'F82BCC66ECF931542D749DC95C67F9ABF1F9A6B69FFD47B3CFE864493C5714A34134397AF0AF7787' +
    'DE3E301346F6368203FD56D9C77A1C32A5BD1664800F925D88E3BF92EAFCA848D26E31BC2FED70E6' +
    '4DE559CFB59C8601C7247799F74AF50FFB6001B7DC83685CCC0B764E7BE4561D560EAF761F8EBE89' +
    '9B9C165259755348BCACA0860AC6F4FC5B795D7326219EDD77D10E4B93B1BC9CE60B7DD180C4D62A' +
    '7F9C7019C1CEEE645A4228DB2AA495186E9717309FD2E52CC66254F82A4F3F84558717BC3A0C758E' +
    '01AD1E78DE83FE3B04A821C0482BB29658AB26375CEF1279BB065E28DF9AC18F575F5C51693ED7ED' +
    '99B0E83CC8FA8BB4B0F756FC9CCCED5DFF88B6B57098ECEDE5E9BAD3E0628EAFA7C2034A4D110272' +
    'BCCFC0CC87A77912333CB4F8ED7EAF9DFB364ECA6D478371C3DEA315DE2199B381A3341F35E72334' +
    'B4281B63D4528D4BD8E26BE7C58316F1EEF6B3ABDB2E3A4E9F884A7ACA877F661D144E8338716877' +
    '897F43196F393CCB1EFFA7B54E414802AB53D53CC917CB1BFBDA21024E34F095AB01ED2330E6A275' +
    'C5149195A37B413E169CB82807137877A374FF11AE02DFB0944A308F805E9F637CDD4F428E6E90FB' +
    'ECCF042EE584E0DB916D82D04A1E52';
  S_SIG =
    '4823427A55790F42CE80C0B75213DAAF2CFC0CF94AEAF32597AAD94F70BA9260019D65758A8AF6F7' +
    '783A0BD7461048426E04ADE9178F60235BB09C65107E1B3B65900D1617821583D8BA3E96EB1C1332' +
    '4E95F7C10DC1EFEFFC33C8554B0C7ACC92D8E83BDDFC7E0DD2B65B2EF421DE0CB827DA71B5D072CE' +
    '6225C91D1B2C5B39EE8F90BA32D62A23847FC6D4847305B672E47BBEA4EAF44FCE4EA121CF2289AE' +
    'E437A71A0566016DF6A5B9411792397F0CFAE11F5101226ACD2B124BCF21ADB3D761F4E803CB8C66' +
    '21496394BA3E9C5791ADBF4F41FE38B44A4122E2991E0F7587A25DC07944646B9F3B95653006640B' +
    '31C44C8FAC70807D6993FB0904502C831C906385E4EA2BCCDCBAE9A2FCDA611B3910FED6DA159BA5' +
    'E2E9FD9591332FC720A183C494A16EC63EDE5801D58AD89127558A78589B725AB3D07A8168571A54' +
    '92253B8D0548669E302FA98F847250E93B30E69BE4CA0C772C17ADBEAC5F0F3498FCA6631C09A805' +
    '852CABCE756AFF371D14BAA69747E6D67476491B0EE12032C2180A7486AE6E254F59081E4F971645' +
    'DFB4DE55FECE847ECCFA35E4ECBB37A96BC94A95775900930CA4AE3915B9A8CA2F15092A41A52577' +
    '42D3846D4D7D213E4CCE92281558A271586D57B118A199214E1728E81B9418F1C1389C01FC589E91' +
    '861DFAFB2EEBF1FA17C44E204A46F4C15C983157A454E7367FD2DB3FE25415426DC195E799EE63E1' +
    'CD75AC3E77E57787CDB4BE6FA88BD225A77E0C78819615E92D9140125CB5D6433EE597B9DE478B8F' +
    '9C461C65E6AB2A5D00367966E795EFFF1AC8AFEAFB98A67037DC3973468A3E4B7489EC1311707994' +
    '95014DD9BC30A6B09EE725983E49CDEF7E91025FFD0150FB31977B93D04DB2750E1E06168F361382' +
    '7222828D0DD7285F9EA570617910DEDBA750F46B0D4A4F7AE99A7E9CCDB2476C1FE33979EFC08B44' +
    '96003D4A2BD6B016D6C5CA48F841A4E5EAC979EA7A20ED6466EBB58B49A19AD49E4CFC444CA36605' +
    '3240036878F351805D8ADA81CB36DCB0B96B7F361A379A545BD3BEDBFEF941485C65A3C53A6E34EB' +
    '7CEA678E820AE080F511533A99BB01CDDF9A30C5DE0ACD91772757C11800AFE653C1ED69DAC6B82D' +
    'F57AD745CDC6DB56C91B251F7E667972AC22150CCAAD14BC43B3EBB0EEA613A74C79170DF20EE9C7' +
    '4D19A60EF9FE4FD06FCE5F262A977356F54DA622332AE3BC52FFCD19F967E4CFA0CBBD0C57C7C609' +
    '1156DF3FF4661B92BE3EEB9574C5060DD979DC9E302424A1A51563100FE1C1B6081964F60D12E2F1' +
    '7E41F6F66B4D68ECD5BBBC3EDB6ACD432B7FC8BA7C7BEE402F3FE7BE559B0B4E5E198777498EFDF1' +
    '508D52520CADF7CF81BB968A4F3B4B0945EA67A0635D8F5C20464FEF6FB1D12A0EF6518309840798' +
    '6D7B737B4C0C91DD6F9C6A096E1CE0EC65D4CDE300A66485CAF19F38DD343E832A6846661F92FF9E' +
    '8D194EB2C3BC932F175BD79B06E373CD80DF7C67B4060B106D9964608D30B4BFFF9EB870D426E4D5' +
    'BCEB39856AD284DF165E4EEE170A47EBC6B7471E26667C4F8AD1190D846230FC3D9B74CFDCB6A59A' +
    '4E19D904C83ECFA0F9A05671AB9789557681EE9FCE6821D6B231955A77C890ABCF3158AFF12D10FE' +
    '92AE123858E787FB9CE8DDEA5D7BCC8A7ECF4C5DA161165880BF12873746DFB5B54B1EAC8B59CC67' +
    '6B36E411E4169381D4A646063E967727883FF972B85915D6F41B3039EE94E95479D88C64056661F7' +
    'D91C354B2D2D01E427386D6F71E9F2CCCABAB7D5A9F91A7E295B6525C60EFF3A08F91037AA8915BC' +
    'ED46AA686851B92AF96D72F0A86B6752E4C339567C31C8EB888BACB6718C92A1006C0F1448AE3806' +
    'D242E9085B385E15F1B98C2A57FCEBA183886559CA01C93E7F9CC14DBCEDA4097F66827376C38E52' +
    '7DD58505042BB3704EE1CAAF57F41B094DA683453F4CA1C5E9C58068F0EFEB22E7DD97B7903DB487' +
    'BDC23BE4DC1E48DE322287DC79763327BB81DABED27CB11CC4F74B726B67D950D0E8A784B3EF6136' +
    'BAF2F0B85D4BC9FD2B42C19DA445B87F7F400DA3113F4A91555A530C61A3CBA7B64F7445739DD42B' +
    'F8AA31189276EBDD79041338324C768DB183BEF84B07F34A6628A8DBB14A1BC24BC1B018AB7575D1' +
    '049582A2F7FD4B3DC68576FC10BB3686690CDCF55CA1E524C5554A293902629941A44D24E638C1B5' +
    'FA09E806A773F8507DBD60701D5D017CF4E7407C07C1084CC1D057F560ECEE3694091E3ADF2D90C1' +
    '6E0DDF2D25977B693D9F908D624A12CB669B05C1E997585A43FE1D92B0059E763F8CC8637BCFB37A' +
    '70C09E4814291DB0041971156711D985F76D620E72E4F731CE8B4EB73C922BB719729771FF0A93AC' +
    '59C38DAAAC51451949D377DF1A95499E1FD95C13F93477EE15431CA0CDE351B396DB98F0E86E017C' +
    '29D562193E21F72714B3AD49787B5084DE084C086DD7BE4857C84E1514465611E01AC1A85FEB125F' +
    'DE520C7B1263FF84C00AC72E5724FE9B976BB2C4B3A20F5CA1ED8E8466F23E5A35745DB14635966E' +
    '0A46D5751D0CAE21B24B54A759B4670A1F9B4B3520C8B35C16B12948486101986EEF14CF595AA6A0' +
    '274C9F20A68874858812E5BAE269771D6B02CD28D1C57C9DB656CF296854D48AB52098FB61EA654D' +
    'CEE4B2CED5DEBE5DF0BB15FA69A6B730466EE482F1DFCCF1ED5FA83A954A495CF53204B2665ECA21' +
    '0E8C018AE1B2682EF5FC3CAF73FC0D9DBD04BB322AD46A234CDD18B785E703F91BA11C92AC41159E' +
    'CB94C1AF0AB2DC1356816834EA353ACDF2D50D07623C3EB8886A05B064334463CDA589D673C5BB4D' +
    '777F4AA4D41377CFCCBCA16C80707EBE1E2E2A172908918D04BECC089131085AE93CE2A58B105EFC' +
    'D2F2A7BF9EA7F134B11F3289479C20747B3CD3C60AF3CE5045110F93E20430C1922F85E9F07E2A3D' +
    '7C26B36D3F0E03BBDE0CAF431331EFF3E9237A05EFF23A9FC81196704B68BBD9DC5223E244FC247D' +
    '568964910912813D3EA6CBEDF13FAD61C9D54B08FCEC135C927CBDD335BACD410BED49177F2F58B1' +
    '67EE50920239C61EF0005633EBF8720436CBE9BD3C9CA81152756AEE8733ECD61157A1FA2138698E' +
    'D4C33D3BCE7B345521CCEAB91FF47CD8B5F971F86284F6863420CFEEFA840499096E0CA95EFAEEBA' +
    '0E001B0FF4B2C07138D6961A768E637C6B26A1D42C5FE8A12AFFDCC7548A75786F66798B1BEDA668' +
    '2A92A52AC73D69710A5F562895D51EBA2738D20730C63CF1215C996C3CBFBAE76A211A1BE9911808' +
    '9CD223E845E9F5FB48FCBD3FE37FF0ABDE32CFB5AD0F37DB6133F4C4B78E76BA39361CC6D4D42E70' +
    '091B63E8A21BDCAAD308455476F1A80E8868B4EE2EF04D98967E5033093923BFF3023BA31BA75F1D' +
    'C86BAC6538C469C1A4A226442276398F01DD09B89233A650FFD4C430D3EAAA41B8DAE035406A07F7' +
    '94DFE8CCF64B34AEFE07CB085CCE2DEA8C498A91F3D5681F81F3B7AFF9EAD72649898655D94DC756' +
    '42FF38E49D2EC75F7A88A436022E0B00ABDD56CE5B08D9FC2C902FEF35F06E1D947D26D3BACED2E5' +
    'F9F48ECEAB304D4C43AF66ECD4CC21F0FEB437016A0C9CF5B1B610608152BA49D4A3763F12F6E642' +
    'DCB4DCBBAC688D33CDF634341C9BAD9E4115A6D16194A9A34DE4219B0A34B45188B25BDAEC50494F' +
    'E598330587E40780C987E86DE429974415BD2D4777A553F16671CE6201E39CA8600579652984DDCC' +
    '5B0781C554D729AC19CF99E333B5D1CDA689ACA332DEA8D45E975517D817CE60AFF28DEBDE2575F4' +
    'E16AA6710179A7D3810122B838A50FC5208584FB5116E3BB6BA72A187C5516ABE5597226CB8ECBE3' +
    'AF59FC1E89B8B219E2E81EF1F42A9375370D8D3759BE7DF315114D09638BAC525E17EC1264F3133F' +
    'D9E229587FFDD889A010ADD51B59B8135D21D312FB4F7C2439C5DBE91CB3D93F869015BFF3ECB6CD' +
    '8DF6F6C457330FD127609B0526B8D07AEFAF62EEE79929702B78B0490BC9FE5F68A9CAA37576E501' +
    '137CEB741EBD535CE3D57F5AC95A0BE2943CA952006F0CEF94736548AB7FC177CE3FC3D57E5827D4' +
    '1FE3E9445037609FAD026958A72F8427CE348BDD1E6818BEC57B116232C6A4BC5116C79C546E98AB' +
    '80A00918121BFE12255877A2313A965E62C6F78044A94B76C98087BC6CD19B6BDFD923B86F7826B6' +
    'F48FD8B9DD4CD5BE25266877F1336113BF5AE9E9EBF1DEF231C7BE77CF644A7433F387AA4873ECC1' +
    'BE22F911012CBDE387D3C4059D170A7C3B85B4551265688831AA61F157693EAEBFC014B3CE05A607' +
    'EA966850836FD069F3F8BFEC68FB973D6D31AD33C4057275F9C8A11AFF42FF7A3D5FDBF40226FA49' +
    'B881A60A6B6DBAA3AF4F0BD746A1931CF507A2CC3895EBF35235FA58D2958ACEDCA2FF0AC11CF48E' +
    '75771D1DF08B1876D8CF579BE966123A3F60EF43B8E531090930E4F7FACD37326ECF24BE2F59995C' +
    'B1D2E95931767B85CF0C299B50BEA2D73E84C554CF0E36AF491E3A46681A47D4F9FA477EC2D0A88B' +
    '7CB8D9971D16320CA4DBA14A8373D4C6BCB3B491115EE54096545D7DB9201967566E49FCD684F014' +
    'D027DBE79C7483D3BEA7E316809D4F55A401AD974E62AF6B66A16ECD6B35DD14B3506C37F2BA6CA1' +
    'ECBB8D07E2C4855FA6A030235569EEB21C623591D805175FC6481F9BA7192AD6BC4C34D948FA2DA5' +
    'F712B6C772135BFBE7F992C1C0C395527E7A950084E72E0DB42347E384376BC62EECC6525DC9C59D' +
    '071646C93151FB8B2CA97326BFA45174537CF7CFD50FF9D3814A4E61CDCFB039BF984AF6B7BF1344' +
    '2C4A3A5075AE7EF6CAC1ED2F37BD45E1E6E8ACE8BC89E3F73502D2D7658F69519A3D09C2BCCCF86F' +
    'BB8238D722B89EFA585844C116B0F774EEEA8EC24162B024865B9828C00330F11A5B484B3F7013F5' +
    '8C00D2E228E72A3DB2604F46F3345F84C1D87D26449A82E13746A8E7671DF2E9AE1FF19893853F19' +
    '3FA327BCCE7633DBB608AF8DD841F2B13AC3A14A1E5ABA2168826CF4D9C7F07E9D440EF40808D469' +
    '6FF6131EF03A72BA82BD03153D1B252CEDCDA889D4645F23EE5ABCEC424471AD25DCD96FC71773F1' +
    '3069901DB4913AB7692119C429109679C116767F8BE7DFCF03E554C32C0DD1B7B26CBA2910027C7D' +
    '2D98937A9B1014A226C35E830B9D49967F02BF5CAB7E44F202CA9FBD88372021DFF4647CA92278EB' +
    '6294F2363A5EFF55A083F57A2441B8D00CA653227575A721673E92F39A88A604F5DED2B130732D29' +
    '449CA0D39EA9144B563DBE425E3C633EB4BD2EA036B6334B19AC71F19ADFCDA56EC8F5B242141FD8' +
    'ED8212C99DE06DF3CCEE6891FE8E3757CF69E627DAA0B92FBD3C68DDDD7D06280522B1A2E4297A41' +
    'F8F11B7219CA4D0515FEF18B4B2825832F691551A3532E8419480C5B946A53B61D9972C9787060EC' +
    '1BABF53CDF110E64B58EC529AE930BC91236B4A163505C0F24E6E7588296A7A259B402763F7B011D' +
    '9F01A516E48CACA492E2F1FFAC0828AA36B7B2BACCE7C241225C920B47682D384F934798D0DEC2CB' +
    '0B3A728CC5DF5468A5D4A4B50AE703993D3B9496FEDF3D8B069F3EBF6ADB50BA382DBA71A9B6CEFE' +
    '8046E25310193D52B85B352C27385AA35DA7CE63FC271B371E5D7C3FF4E0770DADBE55EE52DE0896' +
    '32AA6381023017DC9FB5DC179D214FC67F60F2C164B0D5B506B5EFB5AABD52ABFF0B9F239B7E5042' +
    '161BDBE10A7B4BDA78B61A496E813DA129C10EEA011C28F90AADC949096113C8C234EC1F0367D060' +
    '038E413984E11A1F5BF87A74B40B5CEFF8D54CBAA345340AB1E7A96F5FB8C5129CF247090DFEA219' +
    '4720231A95292C26E9792E7D868A0C3E3279BF425BD5E9FA44C05DE3EC009FF3889540F7C89D1C38' +
    '8E05395F04A62FC6AF841EBC1662D563576B664C8E3CECDB9A461CAFFC098B56B8AC62D36EF147F0' +
    '72B8759FF04DFFF15DBABF6CBDD12A1B4A8944113245A9B6E9EDF5105C68E84380B03D748E29BF8C' +
    'AAD245234E657EF51E8FFD87BE2C7014AC40457C3B5154EB2863A4270A8C7B996C630A31E1A1BA7F' +
    '1B4D78F810254FA52DCABC8A426ACB1022C1B8C061480CDC1C6D5FB6ABB41181A3798B4CE0A5E290' +
    '5045BAE16989ABF7E100BC403A7E9AF53E0A70007985E89185888B02A5DEE012C30008CE52EFB0E1' +
    '5D5B565DFB9034E0388BAA43B7C8550E54A2A36BE7A1400E1E287D80C68E78D63F6394B3B46401A6' +
    '5C6905056DACB423F52F8C539C59D27730612836FFE8EAA2F1B1AF165461D72883015BB0ADC55D95' +
    '4917C908E1D5F6A30F5DBFE6D128259B1FE1B837C3F619FDCD52C035EDA6CDDA3E5FE11FF804B130' +
    '4F22F9EFF54DA870EAB868D9C08EE4A78F0F3C8AE143FDBE77807CD2726DBEBBC59C67747694907F' +
    'A50CB1B20B2F22CFA7646D4A152FEFFDFFEF01B336574213000C2B8DE2E6425457AAACB2DA296278' +
    '7F96A2C5E2232B4992A3AAAFCDDFEC455489A7DFEB060C1545576A84182C505459B1EAFEFF0D1920' +
    '2B3CA2F0FD0000000000000000000000000000060D151F252C353D';
var
  M: TCnMLDSA;
  Priv: TCnMLDSAPrivateKey;
  Pub: TCnMLDSAPublicKey;
  SK, PK, Sig, Msg: TBytes;
  Ctx: AnsiString;
  S: string;
begin
  M := nil;
  Priv := nil;
  Pub := nil;

  try
    M := TCnMLDSA.Create(cmdt87);
    Priv := TCnMLDSAPrivateKey.Create;
    Pub := TCnMLDSAPublicKey.Create;

    SK := HexToBytes(S_SK);
    PK := HexToBytes(S_PK);
    Msg := HexToBytes(S_MSG_T);
    Ctx := HexToAnsiStr(S_CTX_T);

    M.LoadPrivateKeyFromBytes(Priv, SK);
    M.LoadPublicKeyFromBytes(Pub, PK);

    Sig := M.SignBytes(Priv, Msg, Ctx);
    S := BytesToHex(Sig);
    Result := S = S_SIG;
    if not Result then Exit;

    // 随机生成反复测
    M.GenerateKeys(Priv, Pub, '821229E3226BB7A7774BFC3C3593DA9C4DE76255374CC4B13F9F62860E0C3E06');

    SK := M.SavePrivateKeyToBytes(Priv);
    PK := M.SavePublicKeyToBytes(Pub);

    Msg := HexToBytes(S_MSG);
    Sig := M.SignBytes(Priv, Msg, S_CTX, cmhtNone);

    M.LoadPrivateKeyFromBytes(Priv, SK);
    M.LoadPublicKeyFromBytes(Pub, PK);

    Result := M.VerifyBytes(Pub, Msg, Sig, S_CTX, cmhtNone);
    if not Result then Exit;

    Sig := M.SignBytes(Priv, Msg, S_CTX, cmhtSHA256);
    Result := M.VerifyBytes(Pub, Msg, Sig, S_CTX, cmhtSHA256);
    if not Result then Exit;

    Sig := M.SignBytes(Priv, Msg, S_CTX, cmhtSHA512);
    Result := M.VerifyBytes(Pub, Msg, Sig, S_CTX, cmhtSHA512);
    if not Result then Exit;

    Sig := M.SignBytes(Priv, Msg, S_CTX, cmhtSHAKE128);
    Result := M.VerifyBytes(Pub, Msg, Sig, S_CTX, cmhtSHAKE128);
    if not Result then Exit;

    Sig := M.SignBytes(Priv, Msg, S_CTX, cmhtSM3);
    Result := M.VerifyBytes(Pub, Msg, Sig, S_CTX, cmhtSM3);
  finally
    Pub.Free;
    Priv.Free;
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

function TestSHA512224: Boolean;
var
  Dig: TCnSHA512_224Digest;
  Data: TBytes;
begin
  Data := nil;
  Dig := SHA512_224Bytes(Data);
  Result := DataToHex(@Dig[0], SizeOf(TCnSHA512_224Digest)) = '6ED0DD02806FA89E25DE060C19D3AC86CABB87D6A0DDD05C333B84F4';
end;

function TestSHA512224HMac: Boolean;
var
  Dig: TCnSHA512_224Digest;
  S, Data: TBytes;
begin
  S := HexToBytes('FC1EE84F3EE2E8E97098263A797957787D009B88660DB6F1D489E480B7F' +
    'E407107DF65DDDB3329FD50FDE01344E9DC3FB824BE313D237562FCC48268DF38E651AB6A' +
    '2455B9109D3B31D8ABDB6FB2B657F2C3B89E4FEBDB2B351FEE796E5BD5FBD950C5FA41CE0' +
    '371C30D1D194FAA81EA3DF1C3B534BFA171656524BD8261494EF1FA86F09DF449B678EC');
  Data := HexToBytes('61C797D35D5215525B386CC0F0CB1C95');
  SHA512_224Hmac(@S[0], Length(S), @Data[0], Length(Data), Dig);
  Result := DataToHex(@Dig[0], SizeOf(TCnSHA512_224Digest)) = '8F87616404B070B781EB4BCBF9D86E582DBCF91CB02DEE6436125F4C';
end;

function TestSHA512224Update: Boolean;
var
  D1, D2: TCnSHA512_224Digest;
  C: TCnSHA512_224Context;
  S, S1, S2: AnsiString;
begin
  S1 := '0123456789abcdefghi';
  S2 := 'jklmnop';
  S := S1 + S2;

  D1 := SHA512_224StringA(S);
  SHA512_224Init(C);
  SHA512_224Update(C, PAnsiChar(S1), Length(S1));
  SHA512_224Update(C, PAnsiChar(S2), Length(S2));
  SHA512_224Final(C, D2);

  Result := SHA512_224Match(D1, D2);
end;

function TestSHA512256: Boolean;
var
  Dig: TCnSHA512_256Digest;
  Data: TBytes;
begin
  Data := nil;
  Dig := SHA512_256Bytes(Data);
  Result := DataToHex(@Dig[0], SizeOf(TCnSHA512_256Digest)) = 'C672B8D1EF56ED28AB87C3622C5114069BDD3AD7B8F9737498D0C01ECEF0967A';
end;

function TestSHA512256HMac: Boolean;
var
  Dig: TCnSHA512_256Digest;
  S, Data: TBytes;
begin
  S := HexToBytes('92AC89591CA082B1CBF67A215920B48ECA20B7DCD8AB5C9B47CF1C0BCFA' +
    'C332F6BA9B10426AC32615EB50244E3E0AEA4E0FC5B942A547843B1');
  Data := HexToBytes('A981DD57E84798DF88D42DAF61FAE9CF');
  SHA512_256Hmac(@S[0], Length(S), @Data[0], Length(Data), Dig);
  Result := DataToHex(@Dig[0], SizeOf(TCnSHA512_256Digest)) = '351034E057CB2F8AD1C922BCC678262254274FF1B5F8BB7A750AF1DC4340A2A7';
end;

function TestSHA512256Update: Boolean;
var
  D1, D2: TCnSHA512_256Digest;
  C: TCnSHA512_256Context;
  S, S1, S2: AnsiString;
begin
  S1 := '0123456789abcdefghi';
  S2 := 'jklmnop';
  S := S1 + S2;

  D1 := SHA512_256StringA(S);
  SHA512_256Init(C);
  SHA512_256Update(C, PAnsiChar(S1), Length(S1));
  SHA512_256Update(C, PAnsiChar(S2), Length(S2));
  SHA512_256Final(C, D2);

  Result := SHA512_256Match(D1, D2);
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
  Key: TBytes;
begin
  Key := HexToBytes('000102030405060708090A0B0C0D0E0F101112131415161718191A1B1C1D1E1F');
  Dig := BLAKE2SBytes(nil, Key);
  Result := DataToHex(@Dig[0], SizeOf(TCnBLAKE2SDigest)) = '48A8997DA407876B3D79C0D92325AD3B89CBB754D86AB71AEE047AD345FD2C49';
end;

function TestBLAKE2BKey: Boolean;
var
  Dig: TCnBLAKE2BDigest;
  Key: TBytes;
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

function TestReedSolomon: Boolean;
var
  I: Integer;
  D, R, B, Idxs: TBytes;
begin
  SetLength(D, 8);
  for I := 0 to Length(D) - 1 do
    D[I] := 3 * I + 1;

  R := CnCalcReedSolomonCode(D, 12);
  Result := (Length(R) = 12) and CompareBytes(R, D, 8); // 前 8 个字节必须相同

  if not Result then Exit;

  B := Copy(R, 0, 8);
  B := CnVerifyReedSolomonCode(B, Length(R));

  Result := CompareBytes(D, B);
  if not Result then Exit;

  SetLength(Idxs, 8);
  SetLength(B, 8);
  B[0] := R[1]; Idxs[0] := 1;
  B[1] := R[3]; Idxs[1] := 3;
  B[2] := R[4]; Idxs[2] := 4;
  B[3] := R[6]; Idxs[3] := 6;
  B[4] := R[7]; Idxs[4] := 7;
  B[5] := R[8]; Idxs[5] := 8;
  B[6] := R[10]; Idxs[6] := 10;
  B[7] := R[11]; Idxs[7] := 11;

  B := CnVerifyReedSolomonCode(B, Length(R), Idxs);
  Result := CompareBytes(D, B);
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
