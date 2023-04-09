unit CryptoTest;

interface

{$I CnPack.inc}

uses
  SysUtils,
  CnNative, CnBigNumber, CnSM4, CnDES, CnAES, CnAEAD, CnRSA, CnECC, CnSM2, CnSM3,
  CnSM9, CnFNV, CnKDF, CnBase64, CnCRC32;

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
// ================================ SHA1 =======================================
// ================================ SHA2 =======================================
// ================================ SHA3 =======================================
// ================================ Base64 =====================================
// ================================ AEAD =======================================
// ================================ ChaCha20 ===================================
// ================================ Poly1305 ===================================
// ================================ ZUC ========================================
// ================================ TEA ========================================
// ================================ FNV ========================================
// ================================ ECC ========================================
// ================================ SM2 ========================================
// ================================ SM3 ========================================
// ================================ SM9 ========================================
// ================================ RSA ========================================
// ================================ KDF ========================================

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
// ================================ SHA1 =======================================
// ================================ SHA2 =======================================
// ================================ SHA3 =======================================
// ================================ Base64 =====================================
// ================================ AEAD =======================================
// ================================ ChaCha20 ===================================
// ================================ Poly1305 ===================================
// ================================ ZUC ========================================
// ================================ TEA ========================================
// ================================ FNV ========================================
// ================================ ECC ========================================
// ================================ SM2 ========================================
// ================================ SM3 ========================================
// ================================ SM9 ========================================
// ================================ RSA ========================================
// ================================ KDF ========================================

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
// ================================ SHA1 =======================================
// ================================ SHA2 =======================================
// ================================ SHA3 =======================================
// ================================ Base64 =====================================
// ================================ AEAD =======================================
// ================================ ChaCha20 ===================================
// ================================ Poly1305 ===================================
// ================================ ZUC ========================================
// ================================ TEA ========================================
// ================================ FNV ========================================
// ================================ ECC ========================================
// ================================ SM2 ========================================
// ================================ SM3 ========================================
// ================================ SM9 ========================================
// ================================ RSA ========================================
// ================================ KDF ========================================

end.
