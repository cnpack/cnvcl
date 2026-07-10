program TestSEA;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Classes,
  SEATestUnit;

begin
  WriteLn('CnSEA Test Program');
  WriteLn('==================');

  // ===== 第一部分：模多项式生成与 MIT 数据验证 =====
  RunModularPolynomialTest(2,  MIT_L2);
  RunModularPolynomialTest(3,  MIT_L3);
  RunModularPolynomialTest(5,  MIT_L5);
  RunModularPolynomialTest(7,  MIT_L7);
  RunModularPolynomialTest(11, MIT_L11);
  RunModularPolynomialTest(13, MIT_L13);
  RunModularPolynomialTest(17, MIT_L17);

  // ===== 第二部分：SEA 第一阶段功能测试 =====
  TestJInvariant;
  TestPrimeType;
  TestElkiesTrace;

  TestPointCount(2, 2, 17, 'Small Curve F_17');
  TestPointCount(3, 5, 97, 'Medium Curve F_97');
  TestPointCount(2, 3, 1009, 'Larger Curve F_1009');
  TestPointCount(1, 1, 10007, 'Curve F_10007');
  TestPointCount(2, 1, 100003, 'Curve F_100003');
  TestPointCount(3, 8, 999983, 'Curve F_999983');

  // Cross-validate with the most expensive Schoof test case from CryptoTest.pas
  // A=7, B=1, P=6074001169, expected #E = 6074123004
  TestPointCount(7, 1, 6074001169, 'Schoof Largest Test F_6074001169');

  // ===== 48-bit CM curve: y^2 = x^3 + x over p = 16777213^2 + 38^2 =====
  // CM theory gives #E = p + 1 +/- 2a. Independent verification via [#E]P = O.
  // Needs L up to 19. Schoof infeasible at this size.
  TestPointCount48BitCM;

  // ===== 64-bit CM curve: y^2 = x^3 + x over p = 3037000503^2 + 88^2 =====
  // Needs L up to 31 (Phi_29 and Phi_31). WARNING: may take hours.
  TestPointCount64BitCM;

  // ===== Standard curve: secp112r1 (NIST/SECG, 112-bit) =====
  // Smallest standard ECC curve. h=1 so #E = n.
  // SEA only (Schoof too slow at this size)
  TestPointCountHex(
    'DB7C2ABF62E35E668076BEAD2088',  // a
    '659EF8BA043916EEDE8911702B22',  // b
    'DB7C2ABF62E35E668076BEAD208B',  // p
    'DB7C2ABF62E35E7628DFAC6561C5',  // expected #E = n (since h=1)
    'secp112r1 (112-bit standard)');

  WriteLn;
  WriteLn('Done.');
  Readln;
end.
