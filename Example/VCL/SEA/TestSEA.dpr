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

  WriteLn;
  WriteLn('Done.');
  Readln;
end.
