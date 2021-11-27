object FormBigNumber: TFormBigNumber
  Left = 217
  Top = 85
  Width = 1096
  Height = 720
  Caption = 'Big Number Test'
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
  object pgc1: TPageControl
    Left = 8
    Top = 8
    Width = 1057
    Height = 657
    ActivePage = tsBigNumber
    TabOrder = 0
    object tsBigNumber: TTabSheet
      Caption = 'BigNumber'
      object lblNumber1: TLabel
        Left = 16
        Top = 16
        Width = 46
        Height = 13
        Caption = 'Number 1'
      end
      object lblNum2: TLabel
        Left = 16
        Top = 208
        Width = 46
        Height = 13
        Caption = 'Number 2'
      end
      object lblBytes: TLabel
        Left = 528
        Top = 16
        Width = 55
        Height = 13
        Caption = 'Byte Count:'
      end
      object lblShift: TLabel
        Left = 480
        Top = 208
        Width = 24
        Height = 13
        Caption = 'Shift:'
      end
      object lblIntPower: TLabel
        Left = 904
        Top = 120
        Width = 48
        Height = 13
        Caption = 'Exponent:'
      end
      object mmoNum1: TMemo
        Left = 16
        Top = 40
        Width = 873
        Height = 153
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 9
      end
      object mmoNum2: TMemo
        Left = 16
        Top = 256
        Width = 873
        Height = 161
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 30
      end
      object btnGen1: TButton
        Left = 816
        Top = 12
        Width = 75
        Height = 21
        Caption = 'Generate'
        TabOrder = 8
        OnClick = btnGen1Click
      end
      object btnGen2: TButton
        Left = 816
        Top = 204
        Width = 75
        Height = 21
        Caption = 'Generate'
        TabOrder = 19
        OnClick = btnGen2Click
      end
      object mmoResult: TMemo
        Left = 16
        Top = 448
        Width = 873
        Height = 161
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 45
      end
      object btnDup: TButton
        Left = 72
        Top = 204
        Width = 75
        Height = 21
        Caption = 'Copy'
        TabOrder = 10
        OnClick = btnDupClick
      end
      object btnSwap: TButton
        Left = 152
        Top = 204
        Width = 75
        Height = 21
        Caption = 'Swap'
        TabOrder = 11
        OnClick = btnSwapClick
      end
      object btnCompare: TButton
        Left = 232
        Top = 204
        Width = 75
        Height = 21
        Caption = 'Compare'
        TabOrder = 12
        OnClick = btnCompareClick
      end
      object btnInverseNeg1: TButton
        Left = 680
        Top = 12
        Width = 81
        Height = 21
        Caption = 'Inverse Neg'
        TabOrder = 6
        OnClick = btnInverseNeg1Click
      end
      object btnInverseNeg2: TButton
        Left = 712
        Top = 204
        Width = 99
        Height = 21
        Caption = 'Inverse Negative'
        TabOrder = 18
        OnClick = btnInverseNeg2Click
      end
      object cbbDigits: TComboBox
        Left = 584
        Top = 12
        Width = 93
        Height = 21
        ItemHeight = 13
        TabOrder = 5
        Text = '4096'
        Items.Strings = (
          '4096'
          '2048'
          '1024'
          '512'
          '256'
          '128'
          '64'
          '32'
          '16'
          '8'
          '4')
      end
      object btnUAdd: TButton
        Left = 16
        Top = 422
        Width = 75
        Height = 21
        Caption = 'Unsigned Add'
        TabOrder = 31
        OnClick = btnUAddClick
      end
      object btnUsub: TButton
        Left = 96
        Top = 422
        Width = 75
        Height = 21
        Caption = 'Unsigned Sub'
        TabOrder = 32
        OnClick = btnUsubClick
      end
      object btnSignedAdd: TButton
        Left = 176
        Top = 422
        Width = 75
        Height = 21
        Caption = 'Signed Add'
        TabOrder = 33
        OnClick = btnSignedAddClick
      end
      object btnSignedSub: TButton
        Left = 256
        Top = 422
        Width = 75
        Height = 21
        Caption = 'Signed Sub'
        TabOrder = 34
        OnClick = btnSignedSubClick
      end
      object btnShiftRightOne: TButton
        Left = 392
        Top = 204
        Width = 75
        Height = 21
        Caption = 'Shift Right 1'
        TabOrder = 14
        OnClick = btnShiftRightOneClick
      end
      object btnShiftleftOne: TButton
        Left = 312
        Top = 204
        Width = 75
        Height = 21
        Caption = 'Shift Left 1'
        TabOrder = 13
        OnClick = btnShiftleftOneClick
      end
      object seShift: TSpinEdit
        Left = 512
        Top = 204
        Width = 41
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 15
        Value = 2
      end
      object btnShiftRight: TButton
        Left = 632
        Top = 204
        Width = 75
        Height = 21
        Caption = 'Shift Right'
        TabOrder = 17
        OnClick = btnShiftRightClick
      end
      object btnShiftLeft: TButton
        Left = 552
        Top = 204
        Width = 75
        Height = 21
        Caption = 'Shift Left'
        TabOrder = 16
        OnClick = btnShiftLeftClick
      end
      object btnSqr: TButton
        Left = 336
        Top = 422
        Width = 41
        Height = 21
        Caption = 'Sqr'
        TabOrder = 35
        OnClick = btnSqrClick
      end
      object btnMul: TButton
        Left = 384
        Top = 422
        Width = 41
        Height = 21
        Caption = 'Mul'
        TabOrder = 36
        OnClick = btnMulClick
      end
      object btnDiv: TButton
        Left = 432
        Top = 422
        Width = 41
        Height = 21
        Caption = 'Div'
        TabOrder = 37
        OnClick = btnDivClick
      end
      object btnMod: TButton
        Left = 480
        Top = 422
        Width = 41
        Height = 21
        Caption = 'Mod'
        TabOrder = 38
        OnClick = btnModClick
      end
      object btnExp: TButton
        Left = 528
        Top = 422
        Width = 41
        Height = 21
        Caption = 'Exp'
        TabOrder = 39
        OnClick = btnExpClick
      end
      object seExponent: TSpinEdit
        Left = 576
        Top = 422
        Width = 41
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 40
        Value = 2
      end
      object pnlDisplay: TPanel
        Left = 80
        Top = 4
        Width = 137
        Height = 33
        BevelOuter = bvNone
        TabOrder = 0
        object rbHex: TRadioButton
          Left = 16
          Top = 8
          Width = 57
          Height = 17
          Caption = 'Hex'
          Checked = True
          TabOrder = 0
          TabStop = True
          OnClick = rbDecClick
        end
        object rbDec: TRadioButton
          Left = 64
          Top = 8
          Width = 65
          Height = 17
          Caption = 'Dec'
          TabOrder = 1
          OnClick = rbDecClick
        end
      end
      object btnGcd: TButton
        Left = 624
        Top = 422
        Width = 41
        Height = 21
        Caption = 'Gcd'
        TabOrder = 41
        OnClick = btnGcdClick
      end
      object btnGenWord: TButton
        Left = 104
        Top = 230
        Width = 75
        Height = 21
        Caption = 'Geneate Word'
        TabOrder = 21
        OnClick = btnGenWordClick
      end
      object edtWord: TEdit
        Left = 184
        Top = 230
        Width = 121
        Height = 21
        TabOrder = 22
        Text = '0'
      end
      object btnWordAdd: TButton
        Left = 312
        Top = 230
        Width = 75
        Height = 21
        Caption = 'Add Word'
        TabOrder = 23
        OnClick = btnWordAddClick
      end
      object btnSubWord: TButton
        Left = 392
        Top = 230
        Width = 75
        Height = 21
        Caption = 'Sub Word'
        TabOrder = 24
        OnClick = btnSubWordClick
      end
      object btnMulWord: TButton
        Left = 472
        Top = 230
        Width = 75
        Height = 21
        Caption = 'Mul Word'
        TabOrder = 25
        OnClick = btnMulWordClick
      end
      object btnDivWord: TButton
        Left = 552
        Top = 230
        Width = 75
        Height = 21
        Caption = 'Div Word'
        TabOrder = 26
        OnClick = btnDivWordClick
      end
      object btnModWord: TButton
        Left = 632
        Top = 230
        Width = 75
        Height = 21
        Caption = 'Mod Word'
        TabOrder = 27
        OnClick = btnModWordClick
      end
      object btnVerifyDiv: TButton
        Left = 712
        Top = 230
        Width = 97
        Height = 21
        Caption = 'Verify Div Word'
        TabOrder = 28
        OnClick = btnVerifyDivClick
      end
      object btnMultipleMod: TButton
        Left = 672
        Top = 422
        Width = 75
        Height = 21
        Caption = 'Multiple Mod'
        TabOrder = 42
        OnClick = btnMultipleModClick
      end
      object btnPowerMod: TButton
        Left = 752
        Top = 422
        Width = 75
        Height = 21
        Caption = 'Power Mod'
        TabOrder = 43
        OnClick = btnPowerModClick
      end
      object btnIsPrime: TButton
        Left = 816
        Top = 230
        Width = 73
        Height = 21
        Caption = 'Is Prime?'
        TabOrder = 29
        OnClick = btnIsPrimeClick
      end
      object btnGenPrime: TButton
        Left = 456
        Top = 12
        Width = 65
        Height = 21
        Caption = 'Gen Prime'
        TabOrder = 4
        OnClick = btnGenPrimeClick
      end
      object btnJudgeInt: TButton
        Left = 376
        Top = 12
        Width = 73
        Height = 21
        Caption = 'U/Int 32/64?'
        TabOrder = 3
        OnClick = btnJudgeIntClick
      end
      object btnRandRange: TButton
        Left = 768
        Top = 12
        Width = 41
        Height = 21
        Caption = 'Range'
        TabOrder = 7
        OnClick = btnRandRangeClick
      end
      object btnEnterNum1: TButton
        Left = 224
        Top = 12
        Width = 81
        Height = 21
        Caption = 'Enter Num1'
        TabOrder = 1
        OnClick = btnEnterNum1Click
      end
      object btnEnterNum2: TButton
        Left = 16
        Top = 230
        Width = 81
        Height = 21
        Caption = 'Enter Num2 '
        TabOrder = 20
        OnClick = btnEnterNum2Click
      end
      object btnMInverse: TButton
        Left = 832
        Top = 422
        Width = 57
        Height = 21
        Caption = 'Mod Inv'
        TabOrder = 44
        OnClick = btnMInverseClick
      end
      object btnSetUInt64: TButton
        Left = 308
        Top = 12
        Width = 65
        Height = 21
        Caption = 'Set (U)Int64'
        TabOrder = 2
        OnClick = btnSetUInt64Click
      end
      object btnPowerModCompare: TButton
        Left = 904
        Top = 408
        Width = 129
        Height = 25
        Caption = 'Power Mod Compare'
        TabOrder = 46
        OnClick = btnPowerModCompareClick
      end
      object btnMulModCompare: TButton
        Left = 904
        Top = 440
        Width = 129
        Height = 25
        Caption = 'Mul Mod Compare'
        TabOrder = 47
        OnClick = btnMulModCompareClick
      end
      object btnCheckPrime: TButton
        Left = 904
        Top = 328
        Width = 129
        Height = 25
        Caption = 'Check Prime 1'
        TabOrder = 48
        OnClick = btnCheckPrimeClick
      end
      object btnIntPower: TButton
        Left = 904
        Top = 80
        Width = 129
        Height = 25
        Caption = 'Integer Power'
        TabOrder = 49
        OnClick = btnIntPowerClick
      end
      object seIntPower: TSpinEdit
        Left = 976
        Top = 116
        Width = 57
        Height = 22
        MaxValue = 1024
        MinValue = 0
        TabOrder = 50
        Value = 18
      end
      object btnGetTenCount: TButton
        Left = 904
        Top = 48
        Width = 129
        Height = 25
        Caption = '10 Precision'
        TabOrder = 51
        OnClick = btnGetTenCountClick
      end
      object btnCheckPrime2: TButton
        Left = 904
        Top = 368
        Width = 129
        Height = 25
        Caption = 'Check Prime 2'
        TabOrder = 52
        OnClick = btnCheckPrime2Click
      end
      object btnBNCRT: TButton
        Left = 904
        Top = 480
        Width = 129
        Height = 25
        Caption = 'Chinese Remainder '
        TabOrder = 53
        OnClick = btnBNCRTClick
      end
      object btnBNSqrt: TButton
        Left = 904
        Top = 288
        Width = 129
        Height = 25
        Caption = 'Sqrt'
        TabOrder = 54
        OnClick = btnBNSqrtClick
      end
      object btnBNNextPrime: TButton
        Left = 904
        Top = 224
        Width = 129
        Height = 25
        Caption = 'Next Prime'
        TabOrder = 55
        OnClick = btnBNNextPrimeClick
      end
      object btnBNMulKaratsuba: TButton
        Left = 904
        Top = 512
        Width = 129
        Height = 25
        Caption = 'Mul Karatsuba'
        TabOrder = 56
        OnClick = btnBNMulKaratsubaClick
      end
      object btnRoot: TButton
        Left = 904
        Top = 144
        Width = 129
        Height = 25
        Caption = 'Integer Root'
        TabOrder = 57
        OnClick = btnRootClick
      end
      object btnIsPerfectPower: TButton
        Left = 904
        Top = 184
        Width = 129
        Height = 25
        Caption = 'Is Perfect Power'
        TabOrder = 58
        OnClick = btnIsPerfectPowerClick
      end
      object btnComNum: TButton
        Left = 904
        Top = 552
        Width = 129
        Height = 25
        Caption = 'Combinatorial Numbers'
        TabOrder = 59
        OnClick = btnComNumClick
      end
    end
    object tsSparseBigNumberList: TTabSheet
      Caption = 'Sparse BigNumber List'
      ImageIndex = 1
      object btnSBNLTest1: TButton
        Left = 16
        Top = 16
        Width = 75
        Height = 25
        Caption = 'Set Values'
        TabOrder = 0
        OnClick = btnSBNLTest1Click
      end
      object edtSBNL: TEdit
        Left = 112
        Top = 16
        Width = 641
        Height = 21
        TabOrder = 1
      end
      object mmoSBNL: TMemo
        Left = 16
        Top = 56
        Width = 321
        Height = 161
        TabOrder = 2
      end
    end
  end
end
