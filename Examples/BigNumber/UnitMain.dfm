object FormBigNumber: TFormBigNumber
  Left = 138
  Top = 0
  Width = 1222
  Height = 713
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
    Width = 1185
    Height = 673
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
        Top = 84
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
        TabOrder = 10
      end
      object mmoNum2: TMemo
        Left = 16
        Top = 256
        Width = 873
        Height = 161
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 38
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
        TabOrder = 26
        OnClick = btnGen2Click
      end
      object mmoResult: TMemo
        Left = 16
        Top = 448
        Width = 873
        Height = 185
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 60
      end
      object btnDup: TButton
        Left = 72
        Top = 204
        Width = 75
        Height = 21
        Caption = 'Copy'
        TabOrder = 17
        OnClick = btnDupClick
      end
      object btnSwap: TButton
        Left = 152
        Top = 204
        Width = 75
        Height = 21
        Caption = 'Swap'
        TabOrder = 18
        OnClick = btnSwapClick
      end
      object btnCompare: TButton
        Left = 232
        Top = 204
        Width = 75
        Height = 21
        Caption = 'Compare'
        TabOrder = 19
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
        TabOrder = 25
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
        TabOrder = 45
        OnClick = btnUAddClick
      end
      object btnUsub: TButton
        Left = 96
        Top = 422
        Width = 75
        Height = 21
        Caption = 'Unsigned Sub'
        TabOrder = 46
        OnClick = btnUsubClick
      end
      object btnSignedAdd: TButton
        Left = 176
        Top = 422
        Width = 75
        Height = 21
        Caption = 'Signed Add'
        TabOrder = 47
        OnClick = btnSignedAddClick
      end
      object btnSignedSub: TButton
        Left = 256
        Top = 422
        Width = 75
        Height = 21
        Caption = 'Signed Sub'
        TabOrder = 48
        OnClick = btnSignedSubClick
      end
      object btnShiftRightOne: TButton
        Left = 392
        Top = 204
        Width = 75
        Height = 21
        Caption = 'Shift Right 1'
        TabOrder = 21
        OnClick = btnShiftRightOneClick
      end
      object btnShiftleftOne: TButton
        Left = 312
        Top = 204
        Width = 75
        Height = 21
        Caption = 'Shift Left 1'
        TabOrder = 20
        OnClick = btnShiftleftOneClick
      end
      object seShift: TSpinEdit
        Left = 512
        Top = 204
        Width = 41
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 22
        Value = 2
      end
      object btnShiftRight: TButton
        Left = 632
        Top = 204
        Width = 75
        Height = 21
        Caption = 'Shift Right'
        TabOrder = 24
        OnClick = btnShiftRightClick
      end
      object btnShiftLeft: TButton
        Left = 552
        Top = 204
        Width = 75
        Height = 21
        Caption = 'Shift Left'
        TabOrder = 23
        OnClick = btnShiftLeftClick
      end
      object btnSqr: TButton
        Left = 336
        Top = 422
        Width = 41
        Height = 21
        Caption = 'Sqr'
        TabOrder = 49
        OnClick = btnSqrClick
      end
      object btnMul: TButton
        Left = 384
        Top = 422
        Width = 41
        Height = 21
        Caption = 'Mul'
        TabOrder = 50
        OnClick = btnMulClick
      end
      object btnDiv: TButton
        Left = 432
        Top = 422
        Width = 41
        Height = 21
        Caption = 'Div'
        TabOrder = 51
        OnClick = btnDivClick
      end
      object btnMod: TButton
        Left = 480
        Top = 422
        Width = 41
        Height = 21
        Caption = 'Mod'
        TabOrder = 52
        OnClick = btnModClick
      end
      object btnExp: TButton
        Left = 528
        Top = 422
        Width = 41
        Height = 21
        Caption = 'Exp'
        TabOrder = 53
        OnClick = btnExpClick
      end
      object seExponent: TSpinEdit
        Left = 576
        Top = 422
        Width = 41
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 54
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
        TabOrder = 55
        OnClick = btnGcdClick
      end
      object btnGenWord: TButton
        Left = 104
        Top = 230
        Width = 75
        Height = 21
        Caption = 'Geneate Word'
        TabOrder = 29
        OnClick = btnGenWordClick
      end
      object edtWord: TEdit
        Left = 184
        Top = 230
        Width = 121
        Height = 21
        TabOrder = 30
        Text = '0'
      end
      object btnWordAdd: TButton
        Left = 312
        Top = 230
        Width = 75
        Height = 21
        Caption = 'Add Word'
        TabOrder = 31
        OnClick = btnWordAddClick
      end
      object btnSubWord: TButton
        Left = 392
        Top = 230
        Width = 75
        Height = 21
        Caption = 'Sub Word'
        TabOrder = 32
        OnClick = btnSubWordClick
      end
      object btnMulWord: TButton
        Left = 472
        Top = 230
        Width = 75
        Height = 21
        Caption = 'Mul Word'
        TabOrder = 33
        OnClick = btnMulWordClick
      end
      object btnDivWord: TButton
        Left = 552
        Top = 230
        Width = 75
        Height = 21
        Caption = 'Div Word'
        TabOrder = 34
        OnClick = btnDivWordClick
      end
      object btnModWord: TButton
        Left = 632
        Top = 230
        Width = 75
        Height = 21
        Caption = 'Mod Word'
        TabOrder = 35
        OnClick = btnModWordClick
      end
      object btnVerifyDiv: TButton
        Left = 712
        Top = 230
        Width = 97
        Height = 21
        Caption = 'Verify Div Word'
        TabOrder = 36
        OnClick = btnVerifyDivClick
      end
      object btnMultipleMod: TButton
        Left = 672
        Top = 422
        Width = 75
        Height = 21
        Caption = 'Multiple Mod'
        TabOrder = 56
        OnClick = btnMultipleModClick
      end
      object btnPowerMod: TButton
        Left = 752
        Top = 422
        Width = 75
        Height = 21
        Caption = 'Power Mod'
        TabOrder = 57
        OnClick = btnPowerModClick
      end
      object btnIsPrime: TButton
        Left = 816
        Top = 230
        Width = 73
        Height = 21
        Caption = 'Is Prime?'
        TabOrder = 37
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
        TabOrder = 28
        OnClick = btnEnterNum2Click
      end
      object btnMInverse: TButton
        Left = 832
        Top = 422
        Width = 57
        Height = 21
        Caption = 'Mod Inv'
        TabOrder = 58
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
        Top = 286
        Width = 129
        Height = 25
        Caption = 'Power Mod Compare'
        TabOrder = 40
        OnClick = btnPowerModCompareClick
      end
      object btnMulModCompare: TButton
        Left = 904
        Top = 315
        Width = 129
        Height = 25
        Caption = 'Mul Mod Compare'
        TabOrder = 41
        OnClick = btnMulModCompareClick
      end
      object btnCheckPrime: TButton
        Left = 904
        Top = 228
        Width = 129
        Height = 25
        Caption = 'Check Prime 1'
        TabOrder = 27
        OnClick = btnCheckPrimeClick
      end
      object btnIntPower: TButton
        Left = 904
        Top = 44
        Width = 129
        Height = 25
        Caption = 'Integer Power'
        TabOrder = 11
        OnClick = btnIntPowerClick
      end
      object seIntPower: TSpinEdit
        Left = 976
        Top = 80
        Width = 57
        Height = 22
        MaxValue = 1024
        MinValue = 0
        TabOrder = 12
        Value = 18
      end
      object btnGetTenCount: TButton
        Left = 904
        Top = 12
        Width = 129
        Height = 25
        Caption = '10 Precision'
        TabOrder = 9
        OnClick = btnGetTenCountClick
      end
      object btnCheckPrime2: TButton
        Left = 904
        Top = 257
        Width = 129
        Height = 25
        Caption = 'Check Prime 2'
        TabOrder = 39
        OnClick = btnCheckPrime2Click
      end
      object btnBNCRT: TButton
        Left = 904
        Top = 344
        Width = 129
        Height = 25
        Caption = 'Chinese Remainder '
        TabOrder = 42
        OnClick = btnBNCRTClick
      end
      object btnBNSqrt: TButton
        Left = 904
        Top = 199
        Width = 129
        Height = 25
        Caption = 'Sqrt'
        TabOrder = 16
        OnClick = btnBNSqrtClick
      end
      object btnBNNextPrime: TButton
        Left = 904
        Top = 170
        Width = 129
        Height = 25
        Caption = 'Next Prime'
        TabOrder = 15
        OnClick = btnBNNextPrimeClick
      end
      object btnBNMulKaratsuba: TButton
        Left = 904
        Top = 373
        Width = 129
        Height = 25
        Caption = 'Mul Karatsuba'
        TabOrder = 43
        OnClick = btnBNMulKaratsubaClick
      end
      object btnRoot: TButton
        Left = 904
        Top = 112
        Width = 129
        Height = 25
        Caption = 'Integer Root'
        TabOrder = 13
        OnClick = btnRootClick
      end
      object btnIsPerfectPower: TButton
        Left = 904
        Top = 141
        Width = 129
        Height = 25
        Caption = 'Is Perfect Power'
        TabOrder = 14
        OnClick = btnIsPerfectPowerClick
      end
      object btnComNum: TButton
        Left = 904
        Top = 402
        Width = 129
        Height = 25
        Caption = 'Combinatorial Numbers'
        TabOrder = 44
        OnClick = btnComNumClick
      end
      object btnBNAKS: TButton
        Left = 904
        Top = 431
        Width = 129
        Height = 25
        Caption = 'AKS is Prime'
        TabOrder = 59
        OnClick = btnBNAKSClick
      end
      object btnFloatToBigNumber: TButton
        Left = 904
        Top = 464
        Width = 129
        Height = 25
        Caption = 'Float To BigNumber'
        TabOrder = 61
        OnClick = btnFloatToBigNumberClick
      end
      object btnBigNumberToFloat: TButton
        Left = 904
        Top = 496
        Width = 129
        Height = 25
        Caption = 'BigNumber To Float'
        TabOrder = 62
        OnClick = btnBigNumberToFloatClick
      end
      object btnBNEuler: TButton
        Left = 904
        Top = 528
        Width = 129
        Height = 25
        Caption = 'BigNumber Euler'
        TabOrder = 63
        OnClick = btnBNEulerClick
      end
      object btnMulDivFloat: TButton
        Left = 904
        Top = 560
        Width = 129
        Height = 25
        Caption = 'Mul Div Float'
        TabOrder = 64
        OnClick = btnMulDivFloatClick
      end
      object btnNegModInv: TButton
        Left = 904
        Top = 592
        Width = 129
        Height = 25
        Caption = 'Neg Mod Inv'
        TabOrder = 65
        OnClick = btnNegModInvClick
      end
      object btnMontReduct: TButton
        Left = 1040
        Top = 12
        Width = 129
        Height = 25
        Caption = 'Mont Reduct Sample'
        TabOrder = 66
        OnClick = btnMontReductClick
      end
      object btnMontReduct1: TButton
        Left = 1040
        Top = 44
        Width = 129
        Height = 25
        Caption = 'Mont Reduct Big'
        TabOrder = 67
        OnClick = btnMontReduct1Click
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
        Width = 753
        Height = 21
        TabOrder = 1
      end
      object mmoSBNL: TMemo
        Left = 16
        Top = 56
        Width = 161
        Height = 161
        TabOrder = 3
      end
      object chkSparseUseSubMerge: TCheckBox
        Left = 880
        Top = 24
        Width = 121
        Height = 17
        Caption = 'Use Subtract Merge'
        TabOrder = 2
      end
      object edtSparseList2: TEdit
        Left = 552
        Top = 56
        Width = 313
        Height = 21
        TabOrder = 6
      end
      object btnSparseMerge: TButton
        Left = 888
        Top = 56
        Width = 113
        Height = 25
        Caption = 'Merge Sparse'
        TabOrder = 7
        OnClick = btnSparseMergeClick
      end
      object mmoSBNL2: TMemo
        Left = 192
        Top = 56
        Width = 161
        Height = 161
        TabOrder = 4
      end
      object mmoSBNL3: TMemo
        Left = 368
        Top = 56
        Width = 161
        Height = 161
        TabOrder = 5
      end
    end
  end
end
