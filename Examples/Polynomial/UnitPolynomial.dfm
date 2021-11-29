object FormPolynomial: TFormPolynomial
  Left = 241
  Top = 133
  Width = 955
  Height = 601
  Caption = 'Polynomial Test'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object lbl4: TLabel
    Left = 449
    Top = 284
    Width = 5
    Height = 13
    Caption = '/'
  end
  object pgcPoly: TPageControl
    Left = 8
    Top = 8
    Width = 929
    Height = 553
    ActivePage = tsIntegerPolynomial
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object tsIntegerPolynomial: TTabSheet
      Caption = 'Int64 Polynomial'
      object grpIntegerPolynomial: TGroupBox
        Left = 8
        Top = 4
        Width = 905
        Height = 513
        Anchors = [akLeft, akTop, akRight, akBottom]
        Caption = 'Int64 Polynomial'
        TabOrder = 0
        object bvl1: TBevel
          Left = 24
          Top = 68
          Width = 857
          Height = 17
          Shape = bsTopLine
        end
        object lblDeg1: TLabel
          Left = 816
          Top = 124
          Width = 38
          Height = 13
          Caption = 'Degree:'
        end
        object lblDeg2: TLabel
          Left = 816
          Top = 220
          Width = 38
          Height = 13
          Caption = 'Degree:'
        end
        object lblIPEqual: TLabel
          Left = 24
          Top = 256
          Width = 11
          Height = 20
          Caption = '='
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -16
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object bvl2: TBevel
          Left = 24
          Top = 292
          Width = 865
          Height = 17
          Shape = bsTopLine
        end
        object btnIPCreate: TButton
          Left = 24
          Top = 32
          Width = 75
          Height = 21
          Caption = 'To String'
          TabOrder = 0
          OnClick = btnIPCreateClick
        end
        object edtIP1: TEdit
          Left = 128
          Top = 32
          Width = 673
          Height = 21
          TabOrder = 1
        end
        object mmoIP1: TMemo
          Left = 24
          Top = 88
          Width = 777
          Height = 57
          ReadOnly = True
          TabOrder = 3
        end
        object mmoIP2: TMemo
          Left = 24
          Top = 184
          Width = 777
          Height = 57
          ReadOnly = True
          TabOrder = 11
        end
        object btnIP1Random: TButton
          Left = 816
          Top = 88
          Width = 75
          Height = 21
          Caption = 'Random'
          TabOrder = 4
          OnClick = btnIP1RandomClick
        end
        object btnIP2Random: TButton
          Left = 816
          Top = 184
          Width = 75
          Height = 21
          Caption = 'Random'
          TabOrder = 12
          OnClick = btnIP2RandomClick
        end
        object edtIPDeg1: TEdit
          Left = 864
          Top = 120
          Width = 25
          Height = 21
          TabOrder = 5
          Text = '9'
        end
        object edtIPDeg2: TEdit
          Left = 864
          Top = 216
          Width = 25
          Height = 21
          TabOrder = 13
          Text = '7'
        end
        object btnIPAdd: TButton
          Left = 88
          Top = 152
          Width = 75
          Height = 25
          Caption = 'Add'
          TabOrder = 6
          OnClick = btnIPAddClick
        end
        object btnIPSub: TButton
          Left = 192
          Top = 152
          Width = 75
          Height = 25
          Caption = 'Sub'
          TabOrder = 7
          OnClick = btnIPSubClick
        end
        object btnIPMul: TButton
          Left = 296
          Top = 152
          Width = 75
          Height = 25
          Caption = 'Mul'
          TabOrder = 8
          OnClick = btnIPMulClick
        end
        object btnIPDiv: TButton
          Left = 408
          Top = 152
          Width = 75
          Height = 25
          Caption = 'Div && Mod'
          TabOrder = 9
          OnClick = btnIPDivClick
        end
        object edtIP3: TEdit
          Left = 56
          Top = 256
          Width = 833
          Height = 21
          TabOrder = 14
        end
        object btnTestExample1: TButton
          Left = 24
          Top = 312
          Width = 113
          Height = 25
          Caption = 'Galois Test Point 1'
          TabOrder = 15
          OnClick = btnTestExample1Click
        end
        object btnTestExample2: TButton
          Left = 152
          Top = 312
          Width = 113
          Height = 25
          Caption = 'Galois Test Point 2'
          TabOrder = 16
          OnClick = btnTestExample2Click
        end
        object btnTestExample3: TButton
          Left = 280
          Top = 312
          Width = 113
          Height = 25
          Caption = 'Galois Test Power 3'
          TabOrder = 17
          OnClick = btnTestExample3Click
        end
        object btnTestExample4: TButton
          Left = 408
          Top = 312
          Width = 113
          Height = 25
          Caption = 'Galois Test Power 4'
          TabOrder = 18
          OnClick = btnTestExample4Click
        end
        object btnPolyGcd: TButton
          Left = 792
          Top = 312
          Width = 97
          Height = 25
          Caption = 'Test Poly Gcd'
          TabOrder = 21
          OnClick = btnPolyGcdClick
        end
        object btnGaloisTestGcd: TButton
          Left = 536
          Top = 312
          Width = 113
          Height = 25
          Caption = 'Galois Test GCD'
          TabOrder = 19
          OnClick = btnGaloisTestGcdClick
        end
        object btnTestGaloisMI: TButton
          Left = 664
          Top = 312
          Width = 113
          Height = 25
          Caption = 'Galois Test Inverse'
          TabOrder = 20
          OnClick = btnTestGaloisMIClick
        end
        object btnGF28Test1: TButton
          Left = 24
          Top = 360
          Width = 113
          Height = 25
          Caption = 'GF2^8 Test1'
          TabOrder = 22
          OnClick = btnGF28Test1Click
        end
        object btnTestGaloisDiv: TButton
          Left = 152
          Top = 360
          Width = 113
          Height = 25
          Caption = 'Test Galois Div'
          TabOrder = 23
          OnClick = btnTestGaloisDivClick
        end
        object btnTestGaloisDivTime: TButton
          Left = 24
          Top = 408
          Width = 241
          Height = 25
          Caption = 'Test Galois Div Time'
          TabOrder = 29
          OnClick = btnTestGaloisDivTimeClick
        end
        object btnTestGaloisCalc: TButton
          Left = 280
          Top = 360
          Width = 113
          Height = 25
          Caption = 'Test Galois Calc'
          TabOrder = 24
          OnClick = btnTestGaloisCalcClick
        end
        object btnTestHugeDiv: TButton
          Left = 408
          Top = 360
          Width = 113
          Height = 25
          Caption = 'Test Huge Div 1'
          TabOrder = 25
          OnClick = btnTestHugeDivClick
        end
        object btnTestHugeDiv2: TButton
          Left = 536
          Top = 360
          Width = 113
          Height = 25
          Caption = 'Test Huge Div 2'
          TabOrder = 26
          OnClick = btnTestHugeDiv2Click
        end
        object btnTestHugeDiv3: TButton
          Left = 664
          Top = 360
          Width = 113
          Height = 25
          Caption = 'Test Huge Div 3'
          TabOrder = 27
          OnClick = btnTestHugeDiv3Click
        end
        object btnTestPowerMod: TButton
          Left = 792
          Top = 360
          Width = 97
          Height = 25
          Caption = 'Test Power Mod'
          TabOrder = 28
          OnClick = btnTestPowerModClick
        end
        object btnInt64PolySetString: TButton
          Left = 816
          Top = 32
          Width = 75
          Height = 25
          Caption = 'Set String'
          TabOrder = 2
          OnClick = btnInt64PolySetStringClick
        end
        object btnTestBigDiv: TButton
          Left = 280
          Top = 408
          Width = 113
          Height = 25
          Caption = 'A Big Div'
          TabOrder = 30
          OnClick = btnTestBigDivClick
        end
        object btnTestBigGCD: TButton
          Left = 408
          Top = 408
          Width = 113
          Height = 25
          Caption = 'A Big GCD'
          TabOrder = 31
          OnClick = btnTestBigGCDClick
        end
        object btnInt64MulDFT: TButton
          Left = 624
          Top = 152
          Width = 75
          Height = 25
          Caption = 'Mul DFT'
          TabOrder = 10
          OnClick = btnInt64MulDFTClick
        end
        object btnTestInt64SimpleDFT: TButton
          Left = 536
          Top = 408
          Width = 169
          Height = 25
          Caption = 'Test Int64 Simple Mul DFT'
          TabOrder = 32
          OnClick = btnTestInt64SimpleDFTClick
        end
        object btnTestInt64SimpleNTT: TButton
          Left = 720
          Top = 408
          Width = 169
          Height = 25
          Caption = 'Test Int64 Simple Mul NTT'
          TabOrder = 33
          OnClick = btnTestInt64SimpleNTTClick
        end
        object btnIPIsMonic: TButton
          Left = 712
          Top = 152
          Width = 65
          Height = 25
          Caption = 'IsMonic'
          TabOrder = 34
          OnClick = btnIPIsMonicClick
        end
      end
    end
    object tsExtensionEcc: TTabSheet
      Caption = 'Ecc on Galois'
      ImageIndex = 1
      object grpEccGalois: TGroupBox
        Left = 8
        Top = 4
        Width = 897
        Height = 509
        Anchors = [akLeft, akTop, akRight, akBottom]
        Caption = 'Ecc on Galois'
        TabOrder = 0
        object btnGaloisOnCurve: TButton
          Left = 24
          Top = 32
          Width = 129
          Height = 25
          Caption = 'Test 1 Point on Curve'
          TabOrder = 0
          OnClick = btnGaloisOnCurveClick
        end
        object btnEccPointAdd: TButton
          Left = 176
          Top = 32
          Width = 113
          Height = 25
          Caption = 'Test Point Add 1'
          TabOrder = 1
          OnClick = btnEccPointAddClick
        end
        object btnTestEccPointAdd2: TButton
          Left = 312
          Top = 32
          Width = 105
          Height = 25
          Caption = 'Test Point Add 2'
          TabOrder = 2
          OnClick = btnTestEccPointAdd2Click
        end
        object btnTestDivPoly: TButton
          Left = 440
          Top = 32
          Width = 177
          Height = 25
          Caption = 'Test Ecc Division Polynomial 1'
          TabOrder = 3
          OnClick = btnTestDivPolyClick
        end
        object btnTestDivPoly2: TButton
          Left = 640
          Top = 32
          Width = 177
          Height = 25
          Caption = 'Test Ecc Division Polynomial 2'
          TabOrder = 4
          OnClick = btnTestDivPoly2Click
        end
        object btnTestGaloisPoint2: TButton
          Left = 24
          Top = 80
          Width = 129
          Height = 25
          Caption = 'Test ? Point on Curve 1'
          TabOrder = 5
          OnClick = btnTestGaloisPoint2Click
        end
        object btnTestPolyPoint2: TButton
          Left = 176
          Top = 80
          Width = 129
          Height = 25
          Caption = 'Test ? Point on Curve 2'
          TabOrder = 6
          OnClick = btnTestPolyPoint2Click
        end
        object btnTestPolyEccPoint3: TButton
          Left = 320
          Top = 80
          Width = 241
          Height = 25
          Caption = 'Test 4 Points on Curve?'
          TabOrder = 7
          OnClick = btnTestPolyEccPoint3Click
        end
        object btnTestGaloisPolyMulMod: TButton
          Left = 784
          Top = 80
          Width = 105
          Height = 25
          Caption = 'Test Galois MulMod'
          TabOrder = 8
          OnClick = btnTestGaloisPolyMulModClick
        end
        object btnTestGaloisModularInverse1: TButton
          Left = 24
          Top = 128
          Width = 161
          Height = 25
          Caption = 'Test Galois Modular Inverse'
          TabOrder = 9
          OnClick = btnTestGaloisModularInverse1Click
        end
        object btnTestEuclid2: TButton
          Left = 192
          Top = 128
          Width = 161
          Height = 25
          Caption = 'Test Extended Euclid 2'
          TabOrder = 10
          OnClick = btnTestEuclid2Click
        end
        object btnTestExtendEuclid3: TButton
          Left = 360
          Top = 128
          Width = 161
          Height = 25
          Caption = 'Test Extended Euclid 3'
          TabOrder = 11
          OnClick = btnTestExtendEuclid3Click
        end
        object btnTestEccDivisionPoly3: TButton
          Left = 528
          Top = 128
          Width = 177
          Height = 25
          Caption = 'Test Ecc Division Polynomial 3'
          TabOrder = 12
          OnClick = btnTestEccDivisionPoly3Click
        end
        object mmoTestDivisionPolynomial: TMemo
          Left = 24
          Top = 176
          Width = 849
          Height = 313
          ScrollBars = ssBoth
          TabOrder = 14
          WordWrap = False
        end
        object btnGenerateDivisionPolynomial: TButton
          Left = 720
          Top = 128
          Width = 169
          Height = 25
          Caption = 'Generate Division Polynomial'
          TabOrder = 13
          OnClick = btnGenerateDivisionPolynomialClick
        end
      end
    end
    object tsRationalPolynomial: TTabSheet
      Caption = 'Rational Polynomial'
      ImageIndex = 2
      object grpRationalPolynomial: TGroupBox
        Left = 8
        Top = 4
        Width = 905
        Height = 509
        Anchors = [akLeft, akTop, akRight, akBottom]
        Caption = 'Int64 Rational Polynomial'
        TabOrder = 0
        object bvl3: TBevel
          Left = 16
          Top = 72
          Width = 873
          Height = 9
          Shape = bsTopLine
        end
        object lbl1: TLabel
          Left = 425
          Top = 100
          Width = 5
          Height = 13
          Caption = '/'
        end
        object lbl2: TLabel
          Left = 425
          Top = 180
          Width = 5
          Height = 13
          Caption = '/'
        end
        object lbl3: TLabel
          Left = 416
          Top = 208
          Width = 6
          Height = 13
          Caption = '='
        end
        object bvl4: TBevel
          Left = 16
          Top = 280
          Width = 873
          Height = 9
          Shape = bsTopLine
        end
        object btnRP2Point: TButton
          Left = 16
          Top = 24
          Width = 169
          Height = 25
          Caption = 'Calc 2 * Point (x, 1*y) on Ecc'
          TabOrder = 0
          OnClick = btnRP2PointClick
        end
        object edtRationalNominator1: TEdit
          Left = 16
          Top = 96
          Width = 385
          Height = 21
          TabOrder = 4
        end
        object edtRationalDenominator1: TEdit
          Left = 448
          Top = 96
          Width = 385
          Height = 21
          TabOrder = 5
        end
        object btnRationalPolynomialAdd: TButton
          Left = 264
          Top = 136
          Width = 75
          Height = 25
          Caption = 'Add'
          TabOrder = 8
          OnClick = btnRationalPolynomialAddClick
        end
        object btnRationalPolynomialSub: TButton
          Left = 352
          Top = 136
          Width = 75
          Height = 25
          Caption = 'Sub'
          TabOrder = 9
          OnClick = btnRationalPolynomialSubClick
        end
        object btnRationalPolynomialMul: TButton
          Left = 440
          Top = 136
          Width = 75
          Height = 25
          Caption = 'Mul'
          TabOrder = 10
          OnClick = btnRationalPolynomialMulClick
        end
        object btnRationalPolynomialDiv: TButton
          Left = 528
          Top = 136
          Width = 75
          Height = 25
          Caption = 'Div'
          TabOrder = 11
          OnClick = btnRationalPolynomialDivClick
        end
        object chkRationalPolynomialGalois: TCheckBox
          Left = 648
          Top = 140
          Width = 73
          Height = 17
          Caption = 'Galois'
          TabOrder = 13
        end
        object edtRationalPolynomialPrime: TEdit
          Left = 728
          Top = 136
          Width = 105
          Height = 21
          TabOrder = 12
          Text = '97'
        end
        object edtRationalNominator2: TEdit
          Left = 16
          Top = 176
          Width = 385
          Height = 21
          TabOrder = 14
        end
        object edtRationalDenominator2: TEdit
          Left = 448
          Top = 176
          Width = 385
          Height = 21
          TabOrder = 15
        end
        object btnRationalPolynomialGenerate: TButton
          Left = 16
          Top = 136
          Width = 75
          Height = 25
          Caption = 'Generate'
          TabOrder = 6
          OnClick = btnRationalPolynomialGenerateClick
        end
        object edtRationalResultNominator: TEdit
          Left = 20
          Top = 240
          Width = 385
          Height = 21
          TabOrder = 16
        end
        object edtRationalResultDenominator: TEdit
          Left = 452
          Top = 240
          Width = 385
          Height = 21
          TabOrder = 17
        end
        object btnManualOnCurve: TButton
          Left = 224
          Top = 24
          Width = 209
          Height = 25
          Caption = 'Manual Test OnCurve'
          TabOrder = 1
          OnClick = btnManualOnCurveClick
        end
        object btnCheckDivisionPolynomialZero: TButton
          Left = 464
          Top = 24
          Width = 225
          Height = 25
          Caption = 'Check Division Polynomial Zero'
          TabOrder = 2
          OnClick = btnCheckDivisionPolynomialZeroClick
        end
        object btnCalcSimpleEcc: TButton
          Left = 728
          Top = 24
          Width = 161
          Height = 25
          Caption = 'Calc Simple Ecc Int64'
          TabOrder = 3
          OnClick = btnCalcSimpleEccClick
        end
        object mmoEcc: TMemo
          Left = 16
          Top = 296
          Width = 225
          Height = 193
          TabOrder = 18
        end
        object btnCheckRationalAdd: TButton
          Left = 264
          Top = 296
          Width = 209
          Height = 25
          Caption = 'Check Rational Polynomial Add'
          TabOrder = 19
          OnClick = btnCheckRationalAddClick
        end
        object btnTestPiXPolynomial: TButton
          Left = 504
          Top = 296
          Width = 177
          Height = 25
          Caption = 'Test Pi^2 + 2P Polynomial in Ring'
          TabOrder = 20
          OnClick = btnTestPiXPolynomialClick
        end
        object btnTestGaloisEqual: TButton
          Left = 696
          Top = 296
          Width = 137
          Height = 25
          Caption = 'Test Galois Equal'
          TabOrder = 21
          OnClick = btnTestGaloisEqualClick
        end
        object btnRationalSetString: TButton
          Left = 120
          Top = 136
          Width = 75
          Height = 25
          Caption = 'Set String'
          TabOrder = 7
          OnClick = btnRationalSetStringClick
        end
        object btnInt64ComposeRationalRational: TButton
          Left = 264
          Top = 352
          Width = 209
          Height = 25
          Caption = 'Compose Rational using Rational'
          TabOrder = 22
          OnClick = btnInt64ComposeRationalRationalClick
        end
        object btnInt64ComposePolyRational: TButton
          Left = 264
          Top = 400
          Width = 209
          Height = 25
          Caption = 'Compose Polynomial using Rational'
          TabOrder = 24
          OnClick = btnInt64ComposePolyRationalClick
        end
        object btnInt64ComposeRationalPolynomial: TButton
          Left = 264
          Top = 448
          Width = 209
          Height = 25
          Caption = 'Compose Rational using Polynomial'
          TabOrder = 25
          OnClick = btnInt64ComposeRationalPolynomialClick
        end
        object btnCompareRationalMul2Method: TButton
          Left = 504
          Top = 352
          Width = 329
          Height = 25
          Caption = 'Compare Int64 Rational Mul using  Division Polynomial and Add'
          TabOrder = 23
          OnClick = btnCompareRationalMul2MethodClick
        end
      end
    end
    object tsBNPolynomial: TTabSheet
      Caption = 'BigNumber Polynomial'
      ImageIndex = 3
      object grpBNPolynomial: TGroupBox
        Left = 8
        Top = 4
        Width = 905
        Height = 505
        Caption = 'BigNumber Polynomial'
        TabOrder = 0
        object lblBPEqual: TLabel
          Left = 24
          Top = 256
          Width = 11
          Height = 20
          Caption = '='
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -16
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object lblBP1Deg: TLabel
          Left = 816
          Top = 220
          Width = 38
          Height = 13
          Caption = 'Degree:'
        end
        object lblBP2Deg: TLabel
          Left = 816
          Top = 124
          Width = 38
          Height = 13
          Caption = 'Degree:'
        end
        object bvl5: TBevel
          Left = 24
          Top = 292
          Width = 865
          Height = 17
          Shape = bsTopLine
        end
        object btnBNPToString: TButton
          Left = 24
          Top = 32
          Width = 75
          Height = 21
          Caption = 'To String'
          TabOrder = 0
          OnClick = btnBNPToStringClick
        end
        object edtBNPolynomial: TEdit
          Left = 128
          Top = 32
          Width = 673
          Height = 21
          TabOrder = 1
        end
        object mmoBP1: TMemo
          Left = 24
          Top = 88
          Width = 777
          Height = 57
          ReadOnly = True
          TabOrder = 3
        end
        object mmoBP2: TMemo
          Left = 24
          Top = 184
          Width = 777
          Height = 57
          ReadOnly = True
          TabOrder = 10
        end
        object btnBPAdd: TButton
          Left = 88
          Top = 152
          Width = 75
          Height = 25
          Caption = 'Add'
          TabOrder = 6
          OnClick = btnBPAddClick
        end
        object btnBPSub: TButton
          Left = 192
          Top = 152
          Width = 75
          Height = 25
          Caption = 'Sub'
          TabOrder = 7
          OnClick = btnBPSubClick
        end
        object btnBPMul: TButton
          Left = 296
          Top = 152
          Width = 75
          Height = 25
          Caption = 'Mul'
          TabOrder = 8
          OnClick = btnBPMulClick
        end
        object btnBPDivMod: TButton
          Left = 408
          Top = 152
          Width = 75
          Height = 25
          Caption = 'Div && Mod'
          TabOrder = 9
          OnClick = btnBPDivModClick
        end
        object edtBP3: TEdit
          Left = 56
          Top = 256
          Width = 833
          Height = 21
          TabOrder = 13
        end
        object edtBP2Deg: TEdit
          Left = 864
          Top = 216
          Width = 25
          Height = 21
          TabOrder = 12
          Text = '7'
        end
        object btnBP1Rand: TButton
          Left = 816
          Top = 184
          Width = 75
          Height = 21
          Caption = 'Random'
          TabOrder = 11
          OnClick = btnBP1RandClick
        end
        object edtBP1Deg: TEdit
          Left = 864
          Top = 120
          Width = 25
          Height = 21
          TabOrder = 5
          Text = '9'
        end
        object btnBP2Rand: TButton
          Left = 816
          Top = 88
          Width = 75
          Height = 21
          Caption = 'Random'
          TabOrder = 4
          OnClick = btnBP2RandClick
        end
        object btnBNTestExample1: TButton
          Left = 24
          Top = 312
          Width = 113
          Height = 25
          Caption = 'Galois Test Point 1'
          TabOrder = 14
          OnClick = btnBNTestExample1Click
        end
        object btnBNTestExample2: TButton
          Left = 152
          Top = 312
          Width = 113
          Height = 25
          Caption = 'Galois Test Point 2'
          TabOrder = 15
          OnClick = btnBNTestExample2Click
        end
        object btnBNTestExample3: TButton
          Left = 280
          Top = 312
          Width = 113
          Height = 25
          Caption = 'Galois Test Power 3'
          TabOrder = 16
          OnClick = btnBNTestExample3Click
        end
        object btnBNTestExample4: TButton
          Left = 408
          Top = 312
          Width = 113
          Height = 25
          Caption = 'Galois Test Power 4'
          TabOrder = 17
          OnClick = btnBNTestExample4Click
        end
        object btnBNGaloisTestGcd: TButton
          Left = 536
          Top = 312
          Width = 113
          Height = 25
          Caption = 'Galois Test GCD'
          TabOrder = 18
          OnClick = btnBNGaloisTestGcdClick
        end
        object btnBNTestGaloisMI: TButton
          Left = 664
          Top = 312
          Width = 113
          Height = 25
          Caption = 'Galois Test Inverse'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 19
          OnClick = btnBNTestGaloisMIClick
        end
        object btnBNPolyGcd: TButton
          Left = 792
          Top = 312
          Width = 97
          Height = 25
          Caption = 'Test Poly Gcd'
          TabOrder = 20
          OnClick = btnBNPolyGcdClick
        end
        object btnBNGF28Test1: TButton
          Left = 24
          Top = 360
          Width = 113
          Height = 25
          Caption = 'GF2^8 Test1'
          TabOrder = 21
          OnClick = btnBNGF28Test1Click
        end
        object btnBNTestGaloisDiv: TButton
          Left = 152
          Top = 360
          Width = 113
          Height = 25
          Caption = 'Test Galois Div'
          TabOrder = 22
          OnClick = btnBNTestGaloisDivClick
        end
        object btnBNTestGaloisCalc: TButton
          Left = 280
          Top = 360
          Width = 113
          Height = 25
          Caption = 'Test Galois Calc'
          TabOrder = 23
          OnClick = btnBNTestGaloisCalcClick
        end
        object btnBNTestHugeDiv1: TButton
          Left = 408
          Top = 360
          Width = 113
          Height = 25
          Caption = 'Test Huge Div 1'
          TabOrder = 24
          OnClick = btnBNTestHugeDiv1Click
        end
        object btnBNTestHugeDiv2: TButton
          Left = 536
          Top = 360
          Width = 113
          Height = 25
          Caption = 'Test Huge Div 2'
          TabOrder = 25
          OnClick = btnBNTestHugeDiv2Click
        end
        object btnBNTestHugeDiv3: TButton
          Left = 664
          Top = 360
          Width = 113
          Height = 25
          Caption = 'Test Huge Div 3'
          TabOrder = 26
          OnClick = btnBNTestHugeDiv3Click
        end
        object btnBNTestPowerMod: TButton
          Left = 792
          Top = 360
          Width = 97
          Height = 25
          Caption = 'Test Power Mod'
          TabOrder = 27
          OnClick = btnBNTestPowerModClick
        end
        object btnBNTestGaloisDivTime: TButton
          Left = 24
          Top = 408
          Width = 241
          Height = 25
          Caption = 'Test Galois Div Time'
          TabOrder = 28
          OnClick = btnBNTestGaloisDivTimeClick
        end
        object btnBNPolySetString: TButton
          Left = 816
          Top = 32
          Width = 75
          Height = 25
          Caption = 'Set String'
          TabOrder = 2
          OnClick = btnBNPolySetStringClick
        end
      end
    end
    object tsBNEccOnGalois: TTabSheet
      Caption = 'Big Number Ecc on Galois'
      ImageIndex = 4
      object grp1: TGroupBox
        Left = 8
        Top = 8
        Width = 897
        Height = 505
        Anchors = [akLeft, akTop, akRight, akBottom]
        Caption = 'BigNumber Ecc on Galois'
        TabOrder = 0
        object btnBNTestMI1: TButton
          Left = 24
          Top = 128
          Width = 161
          Height = 25
          Caption = 'Test Galois Modular Inverse'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 9
          OnClick = btnBNTestMI1Click
        end
        object btnBNTestEuclid2: TButton
          Left = 192
          Top = 128
          Width = 161
          Height = 25
          Caption = 'Test Extended Euclid 2'
          TabOrder = 10
          OnClick = btnBNTestEuclid2Click
        end
        object btnBNTestGaloisMulMod: TButton
          Left = 784
          Top = 80
          Width = 105
          Height = 25
          Caption = 'Test Galois MulMod'
          TabOrder = 8
          OnClick = btnBNTestGaloisMulModClick
        end
        object btnBNTestEuclid3: TButton
          Left = 360
          Top = 128
          Width = 161
          Height = 25
          Caption = 'Test Extended Euclid 3'
          TabOrder = 11
          OnClick = btnBNTestEuclid3Click
        end
        object btnBNEccDivisionPoly: TButton
          Left = 528
          Top = 128
          Width = 177
          Height = 25
          Caption = 'Test Ecc Division Polynomial 3'
          TabOrder = 12
          OnClick = btnBNEccDivisionPolyClick
        end
        object btnBNGenerateDP: TButton
          Left = 720
          Top = 128
          Width = 169
          Height = 25
          Caption = 'Generate Division Polynomial'
          TabOrder = 13
          OnClick = btnBNGenerateDPClick
        end
        object mmoBNTestDivisionPolynomials: TMemo
          Left = 24
          Top = 176
          Width = 849
          Height = 313
          ScrollBars = ssBoth
          TabOrder = 14
          WordWrap = False
        end
        object btnBNTestDivPoly1: TButton
          Left = 440
          Top = 32
          Width = 177
          Height = 25
          Caption = 'Test Ecc Division Polynomial 1'
          TabOrder = 3
          OnClick = btnBNTestDivPoly1Click
        end
        object btnBNTestDivPoly2: TButton
          Left = 640
          Top = 32
          Width = 177
          Height = 25
          Caption = 'Test Ecc Division Polynomial 2'
          TabOrder = 4
          OnClick = btnBNTestDivPoly2Click
        end
        object btnBNEccOnCurve: TButton
          Left = 24
          Top = 32
          Width = 129
          Height = 25
          Caption = 'Test 1 Point on Curve'
          TabOrder = 0
          OnClick = btnBNEccOnCurveClick
        end
        object btnBNEccPointAdd1: TButton
          Left = 176
          Top = 32
          Width = 113
          Height = 25
          Caption = 'Test Point Add 1'
          TabOrder = 1
          OnClick = btnBNEccPointAdd1Click
        end
        object btnBNEccPointAdd2: TButton
          Left = 312
          Top = 32
          Width = 105
          Height = 25
          Caption = 'Test Point Add 2'
          TabOrder = 2
          OnClick = btnBNEccPointAdd2Click
        end
        object btnBNTestPoly1: TButton
          Left = 24
          Top = 80
          Width = 129
          Height = 25
          Caption = 'Test ? Point on Curve 1'
          TabOrder = 5
          OnClick = btnBNTestPoly1Click
        end
        object btnBNTestPoly2: TButton
          Left = 176
          Top = 80
          Width = 129
          Height = 25
          Caption = 'Test ? Point on Curve 2'
          TabOrder = 6
          OnClick = btnBNTestPoly2Click
        end
        object btnBNTestPoly3: TButton
          Left = 320
          Top = 80
          Width = 241
          Height = 25
          Caption = 'Test 4 Points on Curve?'
          TabOrder = 7
          OnClick = btnBNTestPoly3Click
        end
      end
    end
    object tsBNRationalPolynomial: TTabSheet
      Caption = 'BigNumber Rational Polynomial'
      ImageIndex = 5
      object grpBNRationalPolynomial: TGroupBox
        Left = 8
        Top = 8
        Width = 905
        Height = 505
        Anchors = [akLeft, akTop, akRight, akBottom]
        Caption = 'BigNumber Rational Polynomial'
        TabOrder = 0
        object bvl6: TBevel
          Left = 16
          Top = 72
          Width = 873
          Height = 9
          Shape = bsTopLine
        end
        object lbl5: TLabel
          Left = 425
          Top = 100
          Width = 5
          Height = 13
          Caption = '/'
        end
        object lbl6: TLabel
          Left = 425
          Top = 180
          Width = 5
          Height = 13
          Caption = '/'
        end
        object lbl7: TLabel
          Left = 416
          Top = 208
          Width = 6
          Height = 13
          Caption = '='
        end
        object bvl7: TBevel
          Left = 16
          Top = 280
          Width = 873
          Height = 9
          Shape = bsTopLine
        end
        object btnBNTestDivPoly: TButton
          Left = 464
          Top = 24
          Width = 225
          Height = 25
          Caption = 'Check Division Polynomial Zero'
          TabOrder = 2
          OnClick = btnBNTestDivPolyClick
        end
        object btnBNTestPointAdd: TButton
          Left = 16
          Top = 24
          Width = 169
          Height = 25
          Caption = 'Calc 2 * Point (x, 1*y) on Ecc'
          TabOrder = 0
          OnClick = btnBNTestPointAddClick
        end
        object btnBNTestManualPoint: TButton
          Left = 224
          Top = 24
          Width = 209
          Height = 25
          Caption = 'Manual Test OnCurve'
          TabOrder = 1
          OnClick = btnBNTestManualPointClick
        end
        object edtBNRationalNominator1: TEdit
          Left = 16
          Top = 96
          Width = 385
          Height = 21
          TabOrder = 3
        end
        object edtBNRationalDenominator1: TEdit
          Left = 448
          Top = 96
          Width = 385
          Height = 21
          TabOrder = 4
        end
        object btnBNRationalGenerate: TButton
          Left = 16
          Top = 136
          Width = 75
          Height = 25
          Caption = 'Generate'
          TabOrder = 5
          OnClick = btnBNRationalGenerateClick
        end
        object btnBNRationalAdd: TButton
          Left = 264
          Top = 136
          Width = 75
          Height = 25
          Caption = 'Add'
          TabOrder = 7
          OnClick = btnBNRationalAddClick
        end
        object btnBNRationalSub: TButton
          Left = 352
          Top = 136
          Width = 75
          Height = 25
          Caption = 'Sub'
          TabOrder = 8
          OnClick = btnBNRationalSubClick
        end
        object btnBNRationalMul: TButton
          Left = 440
          Top = 136
          Width = 75
          Height = 25
          Caption = 'Mul'
          TabOrder = 9
          OnClick = btnBNRationalMulClick
        end
        object btnBNRationalDiv: TButton
          Left = 528
          Top = 136
          Width = 75
          Height = 25
          Caption = 'Div'
          TabOrder = 10
          OnClick = btnBNRationalDivClick
        end
        object chkBNRationalGalois: TCheckBox
          Left = 648
          Top = 140
          Width = 73
          Height = 17
          Caption = 'Galois'
          TabOrder = 12
        end
        object edtBNRationalGalois: TEdit
          Left = 728
          Top = 136
          Width = 105
          Height = 21
          TabOrder = 11
          Text = '97'
        end
        object edtBNRationalNominator2: TEdit
          Left = 448
          Top = 176
          Width = 385
          Height = 21
          TabOrder = 14
        end
        object edtBNRationalDenominator2: TEdit
          Left = 16
          Top = 176
          Width = 385
          Height = 21
          TabOrder = 13
        end
        object edtBNRationalResultNominator: TEdit
          Left = 20
          Top = 240
          Width = 385
          Height = 21
          TabOrder = 15
        end
        object edtBNRationalResultDenominator: TEdit
          Left = 452
          Top = 240
          Width = 385
          Height = 21
          TabOrder = 16
        end
        object btnBNTestRationalPointAdd1: TButton
          Left = 264
          Top = 296
          Width = 209
          Height = 25
          Caption = 'Check Rational Polynomial Add'
          TabOrder = 17
          OnClick = btnBNTestRationalPointAdd1Click
        end
        object btnBNTestRationalPointAdd2: TButton
          Left = 504
          Top = 296
          Width = 177
          Height = 25
          Caption = 'Test Pi^2 + 2P Polynomial in Ring'
          TabOrder = 18
          OnClick = btnBNTestRationalPointAdd2Click
        end
        object btnBNTestGaloisEqual: TButton
          Left = 696
          Top = 296
          Width = 137
          Height = 25
          Caption = 'Test Galois Equal'
          TabOrder = 19
          OnClick = btnBNTestGaloisEqualClick
        end
        object btnBNRationalSetString: TButton
          Left = 120
          Top = 136
          Width = 75
          Height = 25
          Caption = 'Set String'
          TabOrder = 6
          OnClick = btnBNRationalSetStringClick
        end
        object btnBNRationalRational: TButton
          Left = 264
          Top = 352
          Width = 209
          Height = 25
          Caption = 'Compose Rational using Rational'
          TabOrder = 20
          OnClick = btnBNRationalRationalClick
        end
        object btnBNPolyRational: TButton
          Left = 264
          Top = 400
          Width = 209
          Height = 25
          Caption = 'Compose Polynomial using Rational'
          TabOrder = 21
          OnClick = btnBNPolyRationalClick
        end
        object btnBNRationalPoly: TButton
          Left = 264
          Top = 448
          Width = 209
          Height = 25
          Caption = 'Compose Rational using Polynomial'
          TabOrder = 22
          OnClick = btnBNRationalPolyClick
        end
      end
    end
    object tsBiInt64Polynomial: TTabSheet
      Caption = 'Bi Int64 Polynomial'
      ImageIndex = 6
      object grpBiInt64Poly: TGroupBox
        Left = 8
        Top = 4
        Width = 905
        Height = 513
        Anchors = [akLeft, akTop, akRight, akBottom]
        Caption = 'Bi Int64 Polynomial'
        TabOrder = 0
        object lblIBPDeg: TLabel
          Left = 816
          Top = 124
          Width = 38
          Height = 13
          Caption = 'Degree:'
        end
        object lblIBP2Deg: TLabel
          Left = 816
          Top = 252
          Width = 38
          Height = 13
          Caption = 'Degree:'
        end
        object lblIBPEqual: TLabel
          Left = 24
          Top = 288
          Width = 11
          Height = 20
          Caption = '='
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -16
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object lblIBPPower: TLabel
          Left = 344
          Top = 158
          Width = 21
          Height = 13
          Caption = 'Exp:'
        end
        object bvl8: TBevel
          Left = 400
          Top = 152
          Width = 17
          Height = 25
          Shape = bsLeftLine
        end
        object bvl9: TBevel
          Left = 664
          Top = 152
          Width = 17
          Height = 25
          Shape = bsLeftLine
        end
        object lblIBPExtract: TLabel
          Left = 672
          Top = 158
          Width = 38
          Height = 13
          Caption = 'Degree:'
        end
        object lblIBPShiftX: TLabel
          Left = 272
          Top = 62
          Width = 24
          Height = 13
          Caption = 'Shift:'
        end
        object bvl10: TBevel
          Left = 24
          Top = 324
          Width = 865
          Height = 17
          Shape = bsTopLine
        end
        object btnBiInt64ToString: TButton
          Left = 24
          Top = 32
          Width = 75
          Height = 21
          Caption = 'To String'
          TabOrder = 0
          OnClick = btnBiInt64ToStringClick
        end
        object edtBIP: TEdit
          Left = 128
          Top = 32
          Width = 673
          Height = 21
          TabOrder = 1
        end
        object btnBiInt64SetString: TButton
          Left = 816
          Top = 32
          Width = 75
          Height = 25
          Caption = 'Set String'
          TabOrder = 2
          OnClick = btnBiInt64SetStringClick
        end
        object mmoIBP1: TMemo
          Left = 24
          Top = 88
          Width = 777
          Height = 57
          ReadOnly = True
          TabOrder = 3
        end
        object btnIBP1Random: TButton
          Left = 816
          Top = 88
          Width = 75
          Height = 21
          Caption = 'Random'
          TabOrder = 4
          OnClick = btnIBP1RandomClick
        end
        object edtIBP1Deg: TEdit
          Left = 864
          Top = 120
          Width = 25
          Height = 21
          TabOrder = 5
          Text = '4'
        end
        object mmoIBP2: TMemo
          Left = 24
          Top = 216
          Width = 777
          Height = 57
          ReadOnly = True
          TabOrder = 11
        end
        object btnIBP2Rand: TButton
          Left = 816
          Top = 216
          Width = 75
          Height = 21
          Caption = 'Random'
          TabOrder = 12
          OnClick = btnIBP2RandClick
        end
        object edtIBP2Deg: TEdit
          Left = 864
          Top = 248
          Width = 25
          Height = 21
          TabOrder = 13
          Text = '4'
        end
        object btnIBPAdd: TButton
          Left = 24
          Top = 152
          Width = 75
          Height = 25
          Caption = 'Add'
          TabOrder = 6
          OnClick = btnIBPAddClick
        end
        object btnIBPSub: TButton
          Left = 104
          Top = 152
          Width = 75
          Height = 25
          Caption = 'Sub'
          TabOrder = 7
          OnClick = btnIBPSubClick
        end
        object btnIBPMul: TButton
          Left = 184
          Top = 152
          Width = 75
          Height = 25
          Caption = 'Mul'
          TabOrder = 8
          OnClick = btnIBPMulClick
        end
        object edtIBP3: TEdit
          Left = 56
          Top = 288
          Width = 833
          Height = 21
          TabOrder = 14
        end
        object btnIBPPower: TButton
          Left = 264
          Top = 152
          Width = 75
          Height = 25
          Caption = 'Power'
          TabOrder = 9
          OnClick = btnIBPPowerClick
        end
        object edtIBPPower: TEdit
          Left = 368
          Top = 154
          Width = 25
          Height = 21
          TabOrder = 10
          Text = '4'
        end
        object btnIBPEvalY: TButton
          Left = 416
          Top = 152
          Width = 75
          Height = 25
          Caption = 'Eval Y'
          TabOrder = 15
          OnClick = btnIBPEvalYClick
        end
        object btnIBPEvalX: TButton
          Left = 496
          Top = 152
          Width = 75
          Height = 25
          Caption = 'Eval X'
          TabOrder = 16
          OnClick = btnIBPEvalXClick
        end
        object btnIBPTranspose: TButton
          Left = 576
          Top = 152
          Width = 75
          Height = 25
          Caption = 'Transpose'
          TabOrder = 17
          OnClick = btnIBPTransposeClick
        end
        object edtIBPExtract: TEdit
          Left = 720
          Top = 154
          Width = 25
          Height = 21
          TabOrder = 18
          Text = '2'
        end
        object btnIBPExtractXY: TButton
          Left = 752
          Top = 152
          Width = 75
          Height = 25
          Caption = 'Extract'
          TabOrder = 19
          OnClick = btnIBPExtractXYClick
        end
        object btnIBPIsMonicX: TButton
          Left = 24
          Top = 184
          Width = 73
          Height = 25
          Caption = 'IsMonicX'
          TabOrder = 20
          OnClick = btnIBPIsMonicXClick
        end
        object edtIBPShiftX: TEdit
          Left = 304
          Top = 58
          Width = 25
          Height = 21
          TabOrder = 21
          Text = '4'
        end
        object btnIBPShiftLeftX: TButton
          Left = 192
          Top = 56
          Width = 75
          Height = 25
          Caption = 'Shift Left'
          TabOrder = 22
          OnClick = btnIBPShiftLeftXClick
        end
        object btnIBPShiftRightX: TButton
          Left = 336
          Top = 56
          Width = 75
          Height = 25
          Caption = 'Shift Right'
          TabOrder = 23
          OnClick = btnIBPShiftRightXClick
        end
        object btnIBPDivModX: TButton
          Left = 104
          Top = 184
          Width = 75
          Height = 25
          Caption = 'DivX && ModX'
          TabOrder = 24
          OnClick = btnIBPDivModXClick
        end
        object btnIBPTestMod: TButton
          Left = 24
          Top = 336
          Width = 113
          Height = 25
          Caption = 'Test Power Mod'
          TabOrder = 25
          OnClick = btnIBPTestModClick
        end
        object btnIBPTestAKSExample: TButton
          Left = 160
          Top = 336
          Width = 113
          Height = 25
          Caption = 'Test AKS Example'
          TabOrder = 26
          OnClick = btnIBPTestAKSExampleClick
        end
        object btnTestModExample: TButton
          Left = 288
          Top = 336
          Width = 113
          Height = 25
          Caption = 'Test Mod Example'
          TabOrder = 27
          OnClick = btnTestModExampleClick
        end
      end
    end
    object tsBNBPolynomial: TTabSheet
      Caption = 'Bi BigNumber Polynomial'
      ImageIndex = 7
      object grpBNBPolynomial: TGroupBox
        Left = 8
        Top = 4
        Width = 905
        Height = 509
        Caption = 'grpBNBPolynomial'
        TabOrder = 0
        object lblBIBNPShiftX: TLabel
          Left = 272
          Top = 62
          Width = 24
          Height = 13
          Caption = 'Shift:'
        end
        object lblBNBIP1: TLabel
          Left = 816
          Top = 124
          Width = 38
          Height = 13
          Caption = 'Degree:'
        end
        object lblBNBPDeg2: TLabel
          Left = 816
          Top = 252
          Width = 38
          Height = 13
          Caption = 'Degree:'
        end
        object lblBNBPEqual: TLabel
          Left = 24
          Top = 288
          Width = 11
          Height = 20
          Caption = '='
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -16
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object bvl11: TBevel
          Left = 24
          Top = 324
          Width = 865
          Height = 17
          Shape = bsTopLine
        end
        object lblBNBPExp: TLabel
          Left = 344
          Top = 158
          Width = 21
          Height = 13
          Caption = 'Exp:'
        end
        object bvl12: TBevel
          Left = 400
          Top = 152
          Width = 17
          Height = 25
          Shape = bsLeftLine
        end
        object bvl13: TBevel
          Left = 664
          Top = 152
          Width = 17
          Height = 25
          Shape = bsLeftLine
        end
        object lblBNBPExtractDegree: TLabel
          Left = 672
          Top = 158
          Width = 38
          Height = 13
          Caption = 'Degree:'
        end
        object btnBNBPToString: TButton
          Left = 24
          Top = 32
          Width = 75
          Height = 21
          Caption = 'To String'
          TabOrder = 0
          OnClick = btnBNBPToStringClick
        end
        object edtBNBP: TEdit
          Left = 128
          Top = 32
          Width = 673
          Height = 21
          TabOrder = 1
        end
        object btnBNBIPSetString: TButton
          Left = 816
          Top = 32
          Width = 75
          Height = 25
          Caption = 'Set String'
          TabOrder = 2
          OnClick = btnBNBIPSetStringClick
        end
        object btnBIBNPShiftLeftX: TButton
          Left = 192
          Top = 56
          Width = 75
          Height = 25
          Caption = 'Shift Left'
          TabOrder = 3
          OnClick = btnBIBNPShiftLeftXClick
        end
        object edtBIBNPShiftX: TEdit
          Left = 304
          Top = 58
          Width = 25
          Height = 21
          TabOrder = 4
          Text = '4'
        end
        object btnBIBNPShiftRightX: TButton
          Left = 336
          Top = 56
          Width = 75
          Height = 25
          Caption = 'Shift Right'
          TabOrder = 5
          OnClick = btnBIBNPShiftRightXClick
        end
        object mmoBNBP1: TMemo
          Left = 24
          Top = 88
          Width = 777
          Height = 57
          ReadOnly = True
          TabOrder = 6
        end
        object btnBNBP1Rand: TButton
          Left = 816
          Top = 88
          Width = 75
          Height = 21
          Caption = 'Random'
          TabOrder = 7
          OnClick = btnBNBP1RandClick
        end
        object edtBNBP1Deg: TEdit
          Left = 864
          Top = 120
          Width = 25
          Height = 21
          TabOrder = 8
          Text = '4'
        end
        object mmoBNBP2: TMemo
          Left = 24
          Top = 216
          Width = 777
          Height = 57
          ReadOnly = True
          TabOrder = 9
        end
        object btnBNBP2Rand: TButton
          Left = 816
          Top = 216
          Width = 75
          Height = 21
          Caption = 'Random'
          TabOrder = 10
          OnClick = btnBNBP2RandClick
        end
        object edtBNBPDeg2: TEdit
          Left = 864
          Top = 248
          Width = 25
          Height = 21
          TabOrder = 11
          Text = '4'
        end
        object edtBNBP3: TEdit
          Left = 56
          Top = 288
          Width = 833
          Height = 21
          TabOrder = 12
        end
        object btnBNBPAdd: TButton
          Left = 24
          Top = 152
          Width = 75
          Height = 25
          Caption = 'Add'
          TabOrder = 13
          OnClick = btnBNBPAddClick
        end
        object btnBNBPSub: TButton
          Left = 104
          Top = 152
          Width = 75
          Height = 25
          Caption = 'Sub'
          TabOrder = 14
          OnClick = btnBNBPSubClick
        end
        object btnBNBPMul: TButton
          Left = 184
          Top = 152
          Width = 75
          Height = 25
          Caption = 'Mul'
          TabOrder = 15
          OnClick = btnBNBPMulClick
        end
        object btnBNBPPower: TButton
          Left = 264
          Top = 152
          Width = 75
          Height = 25
          Caption = 'Power'
          TabOrder = 16
        end
        object edtBNBPExp: TEdit
          Left = 368
          Top = 154
          Width = 25
          Height = 21
          TabOrder = 17
          Text = '4'
        end
        object btnBNBPEvalY: TButton
          Left = 416
          Top = 152
          Width = 75
          Height = 25
          Caption = 'Eval Y'
          TabOrder = 18
        end
        object btnBNBPEvalX: TButton
          Left = 496
          Top = 152
          Width = 75
          Height = 25
          Caption = 'Eval X'
          TabOrder = 19
        end
        object btnBNBPTranspose: TButton
          Left = 576
          Top = 152
          Width = 75
          Height = 25
          Caption = 'Transpose'
          TabOrder = 20
          OnClick = btnBNBPTransposeClick
        end
        object edtBNBPExtactDegree: TEdit
          Left = 720
          Top = 154
          Width = 25
          Height = 21
          TabOrder = 21
          Text = '2'
        end
        object btnBNBPExtract: TButton
          Left = 752
          Top = 152
          Width = 75
          Height = 25
          Caption = 'Extract'
          TabOrder = 22
        end
        object btnBNBPIsMonicX: TButton
          Left = 24
          Top = 184
          Width = 73
          Height = 25
          Caption = 'IsMonicX'
          TabOrder = 23
        end
        object btnBNBPDivXModX: TButton
          Left = 104
          Top = 184
          Width = 75
          Height = 25
          Caption = 'DivX && ModX'
          TabOrder = 24
        end
        object btnTestSetString: TButton
          Left = 24
          Top = 344
          Width = 75
          Height = 25
          Caption = 'Test Set Value'
          TabOrder = 25
          OnClick = btnTestSetStringClick
        end
      end
    end
  end
end
