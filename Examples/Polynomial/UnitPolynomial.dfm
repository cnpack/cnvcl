object FormPolynomial: TFormPolynomial
  Left = 241
  Top = 142
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
    TabOrder = 0
    object tsIntegerPolynomial: TTabSheet
      Caption = 'Integer Polynomial'
      object grpIntegerPolynomial: TGroupBox
        Left = 8
        Top = 4
        Width = 905
        Height = 513
        Caption = 'Integer Polynomial'
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
          Width = 753
          Height = 21
          TabOrder = 1
        end
        object mmoIP1: TMemo
          Left = 24
          Top = 88
          Width = 777
          Height = 57
          ReadOnly = True
          TabOrder = 2
        end
        object mmoIP2: TMemo
          Left = 24
          Top = 184
          Width = 777
          Height = 57
          ReadOnly = True
          TabOrder = 3
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
          TabOrder = 5
          OnClick = btnIP2RandomClick
        end
        object edtIPDeg1: TEdit
          Left = 864
          Top = 120
          Width = 25
          Height = 21
          TabOrder = 6
          Text = '9'
        end
        object edtIPDeg2: TEdit
          Left = 864
          Top = 216
          Width = 25
          Height = 21
          TabOrder = 7
          Text = '7'
        end
        object btnIPAdd: TButton
          Left = 88
          Top = 152
          Width = 75
          Height = 25
          Caption = 'Add'
          TabOrder = 8
          OnClick = btnIPAddClick
        end
        object btnIPSub: TButton
          Left = 192
          Top = 152
          Width = 75
          Height = 25
          Caption = 'Sub'
          TabOrder = 9
          OnClick = btnIPSubClick
        end
        object btnIPMul: TButton
          Left = 296
          Top = 152
          Width = 75
          Height = 25
          Caption = 'Mul'
          TabOrder = 10
          OnClick = btnIPMulClick
        end
        object btnIPDiv: TButton
          Left = 408
          Top = 152
          Width = 75
          Height = 25
          Caption = 'Div && Mod'
          TabOrder = 11
          OnClick = btnIPDivClick
        end
        object edtIP3: TEdit
          Left = 56
          Top = 256
          Width = 833
          Height = 21
          TabOrder = 12
        end
        object btnTestExample1: TButton
          Left = 24
          Top = 312
          Width = 113
          Height = 25
          Caption = 'Galois Test Point 1'
          TabOrder = 13
          OnClick = btnTestExample1Click
        end
        object btnTestExample2: TButton
          Left = 152
          Top = 312
          Width = 113
          Height = 25
          Caption = 'Galois Test Point 2'
          TabOrder = 14
          OnClick = btnTestExample2Click
        end
        object btnTestExample3: TButton
          Left = 280
          Top = 312
          Width = 113
          Height = 25
          Caption = 'Galois Test Power 3'
          TabOrder = 15
          OnClick = btnTestExample3Click
        end
        object btnTestExample4: TButton
          Left = 408
          Top = 312
          Width = 113
          Height = 25
          Caption = 'Galois Test Power 4'
          TabOrder = 16
          OnClick = btnTestExample4Click
        end
        object btnPolyGcd: TButton
          Left = 792
          Top = 312
          Width = 97
          Height = 25
          Caption = 'Test Poly Gcd'
          TabOrder = 17
          OnClick = btnPolyGcdClick
        end
        object btnGaloisTestGcd: TButton
          Left = 536
          Top = 312
          Width = 113
          Height = 25
          Caption = 'Galois Test GCD'
          TabOrder = 18
          OnClick = btnGaloisTestGcdClick
        end
        object btnTestGaloisMI: TButton
          Left = 664
          Top = 312
          Width = 113
          Height = 25
          Caption = 'Galois Test Inverse'
          TabOrder = 19
          OnClick = btnTestGaloisMIClick
        end
        object btnGF28Test1: TButton
          Left = 24
          Top = 360
          Width = 113
          Height = 25
          Caption = 'GF2^8 Test1'
          TabOrder = 20
          OnClick = btnGF28Test1Click
        end
        object btnTestGaloisDiv: TButton
          Left = 152
          Top = 360
          Width = 113
          Height = 25
          Caption = 'Test Galois Div'
          TabOrder = 21
          OnClick = btnTestGaloisDivClick
        end
        object btnTestGaloisDivTime: TButton
          Left = 24
          Top = 408
          Width = 241
          Height = 25
          Caption = 'Test Galois Div Time'
          TabOrder = 22
          OnClick = btnTestGaloisDivTimeClick
        end
        object btnTestGaloisCalc: TButton
          Left = 280
          Top = 360
          Width = 113
          Height = 25
          Caption = 'Test Galois Calc'
          TabOrder = 23
          OnClick = btnTestGaloisCalcClick
        end
        object btnTestHugeDiv: TButton
          Left = 408
          Top = 360
          Width = 113
          Height = 25
          Caption = 'Test Huge Div 1'
          TabOrder = 24
          OnClick = btnTestHugeDivClick
        end
        object btnTestHugeDiv2: TButton
          Left = 536
          Top = 360
          Width = 113
          Height = 25
          Caption = 'Test Huge Div 2'
          TabOrder = 25
          OnClick = btnTestHugeDiv2Click
        end
        object btnTestHugeDiv3: TButton
          Left = 664
          Top = 360
          Width = 113
          Height = 25
          Caption = 'Test Huge Div 3'
          TabOrder = 26
          OnClick = btnTestHugeDiv3Click
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
          TabOrder = 13
          WordWrap = False
        end
        object btnGenerateDivisionPolynomial: TButton
          Left = 720
          Top = 128
          Width = 169
          Height = 25
          Caption = 'Generate Division Polynomial'
          TabOrder = 14
          OnClick = btnGenerateDivisionPolynomialClick
        end
      end
    end
    object tsRationalPolynomial: TTabSheet
      Caption = 'Rational Polynomial'
      ImageIndex = 2
      object grpRationalPolynomial: TGroupBox
        Left = 8
        Top = 8
        Width = 905
        Height = 505
        Caption = 'Rational Polynomial'
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
          Caption = 'Calc 2 * Point (x, 1*y£©on Ecc'
          TabOrder = 0
          OnClick = btnRP2PointClick
        end
        object edtRationalNominator1: TEdit
          Left = 16
          Top = 96
          Width = 385
          Height = 21
          TabOrder = 1
        end
        object edtRationalDenominator1: TEdit
          Left = 448
          Top = 96
          Width = 385
          Height = 21
          TabOrder = 2
        end
        object btnRationalPolynomialAdd: TButton
          Left = 264
          Top = 136
          Width = 75
          Height = 25
          Caption = 'Add'
          TabOrder = 3
          OnClick = btnRationalPolynomialAddClick
        end
        object btnRationalPolynomialSub: TButton
          Left = 352
          Top = 136
          Width = 75
          Height = 25
          Caption = 'Sub'
          TabOrder = 4
          OnClick = btnRationalPolynomialSubClick
        end
        object btnRationalPolynomialMul: TButton
          Left = 440
          Top = 136
          Width = 75
          Height = 25
          Caption = 'Mul'
          TabOrder = 5
          OnClick = btnRationalPolynomialMulClick
        end
        object btnRationalPolynomialDiv: TButton
          Left = 528
          Top = 136
          Width = 75
          Height = 25
          Caption = 'Div'
          TabOrder = 6
          OnClick = btnRationalPolynomialDivClick
        end
        object chkRationalPolynomialGalois: TCheckBox
          Left = 648
          Top = 140
          Width = 73
          Height = 17
          Caption = 'Galois'
          TabOrder = 7
        end
        object edtRationalPolynomialPrime: TEdit
          Left = 728
          Top = 136
          Width = 105
          Height = 21
          TabOrder = 8
          Text = '97'
        end
        object edtRationalNominator2: TEdit
          Left = 16
          Top = 176
          Width = 385
          Height = 21
          TabOrder = 9
        end
        object edtRationalDenominator2: TEdit
          Left = 448
          Top = 176
          Width = 385
          Height = 21
          TabOrder = 10
        end
        object btnRationalPolynomialGenerate: TButton
          Left = 16
          Top = 136
          Width = 75
          Height = 25
          Caption = 'Generate'
          TabOrder = 11
          OnClick = btnRationalPolynomialGenerateClick
        end
        object edtRationalResultNominator: TEdit
          Left = 20
          Top = 240
          Width = 385
          Height = 21
          TabOrder = 12
        end
        object edtRationalResultDenominator: TEdit
          Left = 452
          Top = 240
          Width = 385
          Height = 21
          TabOrder = 13
        end
        object btnManualOnCurve: TButton
          Left = 224
          Top = 24
          Width = 209
          Height = 25
          Caption = 'Manual Test OnCurve'
          TabOrder = 14
          OnClick = btnManualOnCurveClick
        end
        object btnCheckDivisionPolynomialZero: TButton
          Left = 464
          Top = 24
          Width = 225
          Height = 25
          Caption = 'Check Division Polynomial Zero'
          TabOrder = 15
          OnClick = btnCheckDivisionPolynomialZeroClick
        end
        object btnCalcSimpleEcc: TButton
          Left = 728
          Top = 24
          Width = 161
          Height = 25
          Caption = 'Calc Simple Ecc Int64'
          TabOrder = 16
          OnClick = btnCalcSimpleEccClick
        end
        object mmoEcc: TMemo
          Left = 16
          Top = 296
          Width = 225
          Height = 193
          TabOrder = 17
        end
        object btnCheckRationalAdd: TButton
          Left = 264
          Top = 296
          Width = 209
          Height = 25
          Caption = 'Check Rational Polynomial Add'
          TabOrder = 18
          OnClick = btnCheckRationalAddClick
        end
        object btnTestPiXPolynomial: TButton
          Left = 504
          Top = 296
          Width = 177
          Height = 25
          Caption = 'Test Pi^2 + 2P Polynomial in Ring'
          TabOrder = 19
          OnClick = btnTestPiXPolynomialClick
        end
        object btnTestGaloisEqual: TButton
          Left = 696
          Top = 296
          Width = 137
          Height = 25
          Caption = 'Test Galois Equal'
          TabOrder = 20
          OnClick = btnTestGaloisEqualClick
        end
      end
    end
  end
end
