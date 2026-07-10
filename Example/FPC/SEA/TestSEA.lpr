program TestSEA;

{$MODE Delphi}
{$H+}
{$APPTYPE CONSOLE}

// SEATestUnit is shared with the VCL version at ../../VCL/SEA/SEATestUnit.pas
// Compile command (from this directory):
//   fpc -MDelphi -Scghi -O1 \
//     -Fi../../../Source/Common -Fi../../../Source/Crypto \
//     -Fu../../../Source/Common -Fu../../../Source/Crypto \
//     -XR/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX12.1.sdk \
//     TestSEA.lpr

uses
  SysUtils,
  Classes,
  SEATestUnit in '../../VCL/SEA/SEATestUnit.pas';

begin
  WriteLn('CnSEA Test Program');
  WriteLn('==================');

  // ===== Part 1: Modular Polynomial verification against MIT data =====
  RunModularPolynomialTest(2,  MIT_L2);
  RunModularPolynomialTest(3,  MIT_L3);
  RunModularPolynomialTest(5,  MIT_L5);
  RunModularPolynomialTest(7,  MIT_L7);
  RunModularPolynomialTest(11, MIT_L11);
  RunModularPolynomialTest(13, MIT_L13);
  RunModularPolynomialTest(17, MIT_L17);

  // ===== Part 2: SEA Phase 1 functionality tests =====
  TestJInvariant;
  TestPrimeType;
  TestElkiesTrace;

  TestPointCount(2, 2, 17, 'Small Curve F_17');
  TestPointCount(3, 5, 97, 'Medium Curve F_97');
  TestPointCount(2, 3, 1009, 'Larger Curve F_1009');
  TestPointCount(1, 1, 10007, 'Curve F_10007');

  WriteLn;
  WriteLn('Done.');
  Readln;
end.
