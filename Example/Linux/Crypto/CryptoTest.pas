{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2023 CnPack 开发组                       }
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

unit CryptoTest;
{* |<PRE>
================================================================================
* 软件名称：CnPack 密码库
* 单元名称：CnPack 密码库批量测试单元
* 单元作者：刘啸 (liuxiao@cnpack.org)
* 备    注：测试失败的用例会通过 Assert 抛出异常
* 开发平台：PWin7 + Delphi 5.0
* 兼容测试：编译器：Delphi 5~2007 的非 Unicode、Delphi 2009 或以上的 Unicode、FPC 3.2 以上
*           CPU：Intel 32 位、Intel 64 位、ARM 32/64 位、龙芯 64 位
*           OS: Win32、Win64、MacOS64、Linux64
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2023.03.10 V1.0
*               创建单元，实现功能
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes,
  CnNative, CnBigNumber, CnSM4, CnDES, CnAES, CnAEAD, CnRSA, CnECC, CnSM2, CnSM3,
  CnSM9, CnFNV, CnKDF, CnBase64, CnCRC32, CnMD5, CnSHA1, CnSHA2, CnSHA3, CnChaCha20,
  CnPoly1305, CnTEA, CnZUC, CnPrimeNumber;

procedure TestCrypto;
{* 密码库总测试入口}

// ============================== Native =======================================

function TestStrToUInt64: Boolean;
function TestUInt64Div: Boolean;
function TestUInt64Mod: Boolean;

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
function TestBigNumberShiftLeft: Boolean;
function TestBigNumberGetBitsCount: Boolean;
function TestBigNumberShiftRightOne: Boolean;
function TestBigNumberFermatCheckComposite: Boolean;
function TestBigNumberIsProbablyPrime: Boolean;

// ================================ SM4 ========================================

function TestSM4Standard1: Boolean;
function TestSM4Standard2: Boolean;

// ================================ DES ========================================

function TestDESEcb1: Boolean;

// ================================ 3DES =======================================

function Test3DESEcb1: Boolean;

// ================================ AES ========================================

function TestAESEcb128: Boolean;
function TestAESEcb192: Boolean;
function TestAESEcb256: Boolean;

// ================================ CRC ========================================

function TestCRC8CCITT: Boolean;
function TestCRC16CCITT: Boolean;
function TestCRC32: Boolean;
function TestCRC64ECMA: Boolean;

// ================================ MD5 ========================================

function TestMD5: Boolean;
function TestMD5Hmac: Boolean;

// ================================ SHA1 =======================================

function TestSHA1: Boolean;
function TestSHA1HMac: Boolean;

// ================================ SHA2 =======================================

function TestSHA224: Boolean;
function TestSHA224HMac: Boolean;
function TestSHA256: Boolean;
function TestSHA256HMac: Boolean;
function TestSHA384: Boolean;
function TestSHA384HMac: Boolean;
function TestSHA512: Boolean;
function TestSHA512HMac: Boolean;

// ================================ SHA3 =======================================

function TestSHA3_224: Boolean;
function TestSHA3_224HMac: Boolean;
function TestSHA3_256: Boolean;
function TestSHA3_256HMac: Boolean;
function TestSHA3_384: Boolean;
function TestSHA3_384HMac: Boolean;
function TestSHA3_512: Boolean;
function TestSHA3_512HMac: Boolean;

// ================================ Base64 =====================================

function TestBase64: Boolean;

// ================================ AEAD =======================================

function TestAEADAESCCM: Boolean;
function TestAEADSM4CCM: Boolean;
function TestAEADAES128GCM: Boolean;
function TestAEADAES192GCM: Boolean;
function TestAEADAES256GCM: Boolean;
function TestAEADSM4GCM: Boolean;

// ================================ ChaCha20 ===================================

function TestChaCha20: Boolean;

// ================================ Poly1305 ===================================

function TestPoly1305: Boolean;

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

// ================================ TEA ========================================

function TestTea: Boolean;
function TestXTea: Boolean;
function TestXXTea: Boolean;

// ================================ FNV ========================================

function TestFNV1: Boolean;
function TestFNV1a: Boolean;

// ================================ ECC ========================================
// ================================ SM2 ========================================

function TestSM21: Boolean;
function TestSM22: Boolean;

// ================================ SM3 ========================================

function TestSM3: Boolean;
function TestSM3HMac: Boolean;

// ================================ SM9 ========================================
// ================================ RSA ========================================

function TestRSA1: Boolean;
function TestRSA2: Boolean;

// ================================ KDF ========================================

function TestKDFSM2SM9: Boolean;

// ================================ Prime Number ===============================

function TestPrimeNumber1: Boolean;
function TestPrimeNumber2: Boolean;

// ================================ 25519 ========================================

implementation

procedure TestCrypto;
begin
  Writeln('Crypto Test Start...');

// ============================== Native =======================================

  Assert(TestStrToUInt64, 'TestStrToUInt64');
  Assert(TestUInt64Div, 'TestUInt64Div');
  Assert(TestUInt64Mod, 'TestUInt64Mod');

// ============================== BigNumber ====================================

  Assert(TestBigNumberHex, 'TestBigNumberHex');
  Assert(TestBigNumberDec, 'TestBigNumberDec');
  Assert(TestBigNumberExpandWord, 'TestBigNumberExpandWord');
  Assert(TestBigNumberModWord, 'TestBigNumberModWord');
  Assert(TestBigNumberMulWord, 'TestBigNumberMulWord');
  Assert(TestBigNumberDivWord, 'TestBigNumberDivWord');
  Assert(TestBigNumberUnsignedAdd, 'TestBigNumberUnsignedAdd');
  Assert(TestBigNumberPowerMod, 'TestBigNumberPowerMod');
  Assert(TestBigNumberDiv, 'TestBigNumberDiv');
  Assert(TestBigNumberShiftLeft, 'TestBigNumberShiftLeft');
  Assert(TestBigNumberGetBitsCount, 'TestBigNumberGetBitsCount');
  Assert(TestBigNumberShiftRightOne, 'TestBigNumberShiftRightOne');
  Assert(TestBigNumberFermatCheckComposite, 'TestBigNumberFermatCheckComposite');
  Assert(TestBigNumberIsProbablyPrime, 'TestBigNumberIsProbablyPrime');

// ================================ SM4 ========================================

  Assert(TestSM4Standard1, 'TestSM4Standard1');
  Assert(TestSM4Standard2, 'TestSM4Standard2');

// ================================ DES ========================================

  Assert(TestDESEcb1, 'TestDESEcb1');

// ================================ 3DES =======================================

  Assert(Test3DESEcb1, 'Test3DESEcb1');

// ================================ AES ========================================

  Assert(TestAESEcb128, 'TestAESEcb128');
  Assert(TestAESEcb192, 'TestAESEcb192');
  Assert(TestAESEcb256, 'TestAESEcb256');

// ================================ CRC ========================================

  Assert(TestCRC8CCITT, 'TestCRC8CCITT');
  Assert(TestCRC16CCITT, 'TestCRC16CCITT');
  Assert(TestCRC32, 'TestCRC32');
  Assert(TestCRC64ECMA, 'TestCRC64ECMA');

// ================================ MD5 ========================================

  Assert(TestMD5, 'TestMD5');
  Assert(TestMD5Hmac, 'TestMD5Hmac');

// ================================ SHA1 =======================================

  Assert(TestSHA1, 'TestSHA1');
  Assert(TestSHA1Hmac, 'TestSHA1Hmac');

// ================================ SHA2 =======================================

  Assert(TestSHA224, 'TestSHA224');
  Assert(TestSHA224HMac, 'TestSHA224HMac');
  Assert(TestSHA256, 'TestSHA256');
  Assert(TestSHA256HMac, 'TestSHA256HMac');
  Assert(TestSHA384, 'TestSHA384');
  Assert(TestSHA384HMac, 'TestSHA384HMac');
  Assert(TestSHA512, 'TestSHA512');
  Assert(TestSHA512HMac, 'TestSHA512HMac');

// ================================ SHA3 =======================================

  Assert(TestSHA3_224, 'TestSHA3_224');
  Assert(TestSHA3_224HMac, 'TestSHA3_224HMac');
  Assert(TestSHA3_256, 'TestSHA3_256');
  Assert(TestSHA3_256HMac, 'TestSHA3_256HMac');
  Assert(TestSHA3_384, 'TestSHA3_384');
  Assert(TestSHA3_384HMac, 'TestSHA3_384HMac');
  Assert(TestSHA3_512, 'TestSHA3_512');
  Assert(TestSHA3_512HMac, 'TestSHA3_512HMac');

// ================================ Base64 =====================================

  Assert(TestBase64, 'TestBase64');

// ================================ AEAD =======================================

  Assert(TestAEADAESCCM, 'TestAEADAESCCM');
  Assert(TestAEADSM4CCM, 'TestAEADSM4CCM');
  Assert(TestAEADAES128GCM, 'TestAEADAES128GCM');
  Assert(TestAEADAES192GCM, 'TestAEADAES192GCM');
  Assert(TestAEADAES256GCM, 'TestAEADAES256GCM');

// ================================ ChaCha20 ===================================

  Assert(TestChaCha20, 'TestChaCha20');

// ================================ Poly1305 ===================================

  Assert(TestPoly1305, 'TestPoly1305');

// ================================ ZUC ========================================

  Assert(TestZUC1, 'TestZUC1');
  Assert(TestZUC2, 'TestZUC2');
  Assert(TestZUC3, 'TestZUC3');
  Assert(TestZUC4, 'TestZUC4');
  Assert(TestZUCEEA31, 'TestZUCEEA31');
  Assert(TestZUCEEA32, 'TestZUCEEA32');
  Assert(TestZUCEEA33, 'TestZUCEEA33');
  Assert(TestZUCEIA31, 'TestZUCEIA31');
  Assert(TestZUCEIA32, 'TestZUCEIA32');
  Assert(TestZUCEIA33, 'TestZUCEIA33');

// ================================ TEA ========================================

  Assert(TestTea, 'TestTea');
  Assert(TestXTea, 'TestXTea');
  Assert(TestXXTea, 'TestXXTea');

// ================================ FNV ========================================

  Assert(TestFNV1, 'TestFNV1');
  Assert(TestFNV1a, 'TestFNV1a');

// ================================ ECC ========================================
// ================================ SM2 ========================================

  Assert(TestSM21, 'TestSM21');
  Assert(TestSM22, 'TestSM22');

// ================================ SM3 ========================================

  Assert(TestSM3, 'TestSM3');
  Assert(TestSM3Hmac, 'TestSM3Hmac');

// ================================ SM9 ========================================
// ================================ RSA ========================================

  Assert(TestRSA1, 'TestRSA1');
  Assert(TestRSA2, 'TestRSA2');

// ================================ KDF ========================================

  Assert(TestKDFSM2SM9, 'TestKDFSM2SM9');

// ================================ Prime Number ===============================

  Assert(TestPrimeNumber1, 'TestPrimeNumber1');
  Assert(TestPrimeNumber2, 'TestPrimeNumber2');

// ================================ 25519 ======================================

  Writeln('Crypto Test End.');
end;

// ============================== Native =======================================

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
    PCnBigNumberElementArray(T.D)^[0] := $0F73D4B9F147A700;
    PCnBigNumberElementArray(T.D)^[1] := $05D72BCFF78BBB54;
    PCnBigNumberElementArray(T.D)^[2] := $074D5382782E0E84;
    PCnBigNumberElementArray(T.D)^[3] := $07A20D1E34E475C2;
    PCnBigNumberElementArray(T.D)^[4] := $0CA4A192F7331A65;
    PCnBigNumberElementArray(T.D)^[5] := $0586C66DE2BD9685;
    PCnBigNumberElementArray(T.D)^[6] := $0BACACDE82782B14;
    PCnBigNumberElementArray(T.D)^[7] := $0F8DDBF39D15FB5B;

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

    if not Result then
      Exit;

    T.SetDec('111757582461902544929520711250223739903');
    W := 1000000000;
    R := BigNumberModWord(T, W);
    Result := R = 223739903;

    if not Result then
      Exit;

    T.SetHex('0C7D4FAEC98EC3DF');
    W := $6F6C929F;
    R := BigNumberModWord(T, W);
    Result := R = 1802899775;

    if not Result then
      Exit;

    T.SetDec('12345667296');
    W := 100000;
    R := BigNumberModWord(T, W); // Win32 下居然出错等于 0，后已修复
    Result := R = 67296;

    if not Result then
      Exit;

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

// ================================ SM4 ========================================

function TestSM4Standard1: Boolean;
var
  S: AnsiString;
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
  S: AnsiString;
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

// ================================ DES ========================================

function TestDESEcb1: Boolean;
var
  S: AnsiString;
  KeyBytes, ResBytes, DataBytes: TBytes;
begin
  S := '0123456789ABCDEF';
  DataBytes := HexToBytes(S);
  KeyBytes := HexToBytes('133457799BBCDFF1');
  ResBytes := DESEncryptEcbBytes(KeyBytes, DataBytes);
  Result := BytesToHex(ResBytes) = '85E813540F0AB405';
end;

// ================================ 3DES =======================================

function Test3DESEcb1: Boolean;
var
  S: AnsiString;
  KeyBytes, ResBytes, DataBytes: TBytes;
begin
  S := '0123456789ABCDEF';
  DataBytes := HexToBytes(S);
  KeyBytes := HexToBytes('9BBCDFF1AABBCCDD');
  ResBytes := TripleDESEncryptEcbBytes(KeyBytes, DataBytes);
  Result := BytesToHex(ResBytes) = '119102AA7D6000EE';
end;

// ================================ AES ========================================

function TestAESEcb128: Boolean;
var
  S: AnsiString;
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
  S: AnsiString;
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
  S: AnsiString;
  KeyBytes, ResBytes, DataBytes: TBytes;
begin
  S := '0123456789ABCDEF0123456789ABCDEF';
  DataBytes := HexToBytes(S);
  KeyBytes := HexToBytes('603DEB1015CA71BE2B73AEF0857D77811F352C073B6108D72D9810A30914DFF4');
  ResBytes := AESEncryptEcbBytes(DataBytes, KeyBytes, kbt256);
  Result := BytesToHex(ResBytes) = 'D71F96DEF80F6F19F80461CAEB8BE29F';
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

// ================================ Base64 =====================================

function TestBase64: Boolean;
var
  Res: string;
  Data: TBytes;
begin
  Data := HexToBytes('000102030405060708090A0B0C0D0E0F32333425');
  if ECN_BASE64_OK = Base64Encode(Data, Res) then
    Result := Res = 'AAECAwQFBgcICQoLDA0ODzIzNCU='
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

// ================================ ChaCha20 ===================================

function TestChaCha20: Boolean;
var
  S, SKey, SNonce: AnsiString;
  Key: TCnChaChaKey;
  Nonce: TCnChaChaNonce;
  EnRes, DeRes: TBytes;
begin
  SKey := '000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f';
  SNonce := '000000000000004a00000000';

  HexToData(SKey, @Key[0]);
  HexToData(SNonce, @Nonce[0]);

  S := 'Ladies and Gentlemen of the class of ''99: If I could offer you only one tip for the future, sunscreen would be it.';
  SetLength(EnRes, Length(S));

  ChaCha20EncryptData(Key, Nonce, @S[1], Length(S), @EnRes[0]);
  Result := BytesToHex(EnRes) = '6E2E359A2568F98041BA0728DD0D6981E97E7AEC1D4360C20A27AFCCFD9FAE0BF91B65C5524733AB8F593DABCD62B3571639D624E65152AB8F530C359F0861D807CA0DBF500D6A6156A38E088A22B65E52BC514D16CCF806818CE91AB77937365AF90BBF74A35BE6B40B8EEDF2785E42874D';
  if not Result then
    Exit;

  DeRes := ChaCha20DecryptBytes(Key, Nonce, EnRes);
  Result := (DeRes <> nil) and CompareMem(@S[1], @DeRes[0], Length(DeRes));
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

// ================================ ECC ========================================
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

  Sig.Free;
  Pub.Free;
  Priv.Free;
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
    DeStream.Free;
  end;

  EnStream.Free;
  Pub.Free;
  Priv.Free;
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

// ================================ SM9 ========================================
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

// ================================ KDF ========================================

function TestKDFSM2SM9: Boolean;
var
  Pass, Res: TBytes;
begin
  Pass := HexToBytes('57E7B63623FAE5F08CDA468E872A20AFA03DED41BF1403770E040DC83AF31A67991F2B01EBF9EFD8881F0A0493000603');
  Res := CnSM2SM9KDF(Pass, 19);
  Result := DataToHex(@Res[0], Length(Res)) = '046B04A9ADF53B389B9E2AAFB47D90F4D08978';
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

// ================================ 25519 ========================================


end.
