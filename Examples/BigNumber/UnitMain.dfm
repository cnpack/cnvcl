object FormBigNumber: TFormBigNumber
  Left = 79
  Top = 41
  Width = 920
  Height = 656
  Caption = 'Big Number Test'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
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
    Left = 472
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
  object mmoNum1: TMemo
    Left = 16
    Top = 40
    Width = 873
    Height = 153
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 4
  end
  object mmoNum2: TMemo
    Left = 16
    Top = 256
    Width = 873
    Height = 161
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 22
  end
  object btnGen1: TButton
    Left = 816
    Top = 12
    Width = 75
    Height = 21
    Caption = 'Generate'
    TabOrder = 3
    OnClick = btnGen1Click
  end
  object btnGen2: TButton
    Left = 816
    Top = 204
    Width = 75
    Height = 21
    Caption = 'Generate'
    TabOrder = 14
    OnClick = btnGen2Click
  end
  object mmoResult: TMemo
    Left = 16
    Top = 448
    Width = 873
    Height = 161
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 34
  end
  object btnDup: TButton
    Left = 72
    Top = 204
    Width = 75
    Height = 21
    Caption = 'Copy'
    TabOrder = 5
    OnClick = btnDupClick
  end
  object btnSwap: TButton
    Left = 152
    Top = 204
    Width = 75
    Height = 21
    Caption = 'Swap'
    TabOrder = 6
    OnClick = btnSwapClick
  end
  object btnCompare: TButton
    Left = 232
    Top = 204
    Width = 75
    Height = 21
    Caption = 'Compare'
    TabOrder = 7
    OnClick = btnCompareClick
  end
  object btnInverseNeg1: TButton
    Left = 712
    Top = 12
    Width = 99
    Height = 21
    Caption = 'Inverse Negative'
    TabOrder = 2
    OnClick = btnInverseNeg1Click
  end
  object btnInverseNeg2: TButton
    Left = 712
    Top = 204
    Width = 99
    Height = 21
    Caption = 'Inverse Negative'
    TabOrder = 13
    OnClick = btnInverseNeg2Click
  end
  object cbbDigits: TComboBox
    Left = 532
    Top = 12
    Width = 145
    Height = 21
    ItemHeight = 13
    TabOrder = 1
    Text = '4096'
    Items.Strings = (
      '4096'
      '1024'
      '512'
      '256'
      '128'
      '64'
      '32'
      '8'
      '4')
  end
  object btnUAdd: TButton
    Left = 16
    Top = 422
    Width = 75
    Height = 21
    Caption = 'Unsigned Add'
    TabOrder = 23
    OnClick = btnUAddClick
  end
  object btnUsub: TButton
    Left = 96
    Top = 422
    Width = 75
    Height = 21
    Caption = 'Unsigned Sub'
    TabOrder = 24
    OnClick = btnUsubClick
  end
  object btnSignedAdd: TButton
    Left = 176
    Top = 422
    Width = 75
    Height = 21
    Caption = 'Signed Add'
    TabOrder = 25
    OnClick = btnSignedAddClick
  end
  object btnSignedSub: TButton
    Left = 256
    Top = 422
    Width = 75
    Height = 21
    Caption = 'Signed Sub'
    TabOrder = 26
    OnClick = btnSignedSubClick
  end
  object btnShiftRightOne: TButton
    Left = 392
    Top = 204
    Width = 75
    Height = 21
    Caption = 'Shift Right 1'
    TabOrder = 9
    OnClick = btnShiftRightOneClick
  end
  object btnShiftleftOne: TButton
    Left = 312
    Top = 204
    Width = 75
    Height = 21
    Caption = 'Shift Left 1'
    TabOrder = 8
    OnClick = btnShiftleftOneClick
  end
  object seShift: TSpinEdit
    Left = 512
    Top = 204
    Width = 41
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 10
    Value = 2
  end
  object btnShiftRight: TButton
    Left = 632
    Top = 204
    Width = 75
    Height = 21
    Caption = 'Shift Right'
    TabOrder = 12
    OnClick = btnShiftRightClick
  end
  object btnShiftLeft: TButton
    Left = 552
    Top = 204
    Width = 75
    Height = 21
    Caption = 'Shift Left'
    TabOrder = 11
    OnClick = btnShiftLeftClick
  end
  object btnSqr: TButton
    Left = 336
    Top = 422
    Width = 41
    Height = 21
    Caption = 'Sqr'
    TabOrder = 27
    OnClick = btnSqrClick
  end
  object btnMul: TButton
    Left = 384
    Top = 422
    Width = 41
    Height = 21
    Caption = 'Mul'
    TabOrder = 28
    OnClick = btnMulClick
  end
  object btnDiv: TButton
    Left = 432
    Top = 422
    Width = 41
    Height = 21
    Caption = 'Div'
    TabOrder = 29
    OnClick = btnDivClick
  end
  object btnMod: TButton
    Left = 480
    Top = 422
    Width = 41
    Height = 21
    Caption = 'Mod'
    TabOrder = 30
    OnClick = btnModClick
  end
  object btnExp: TButton
    Left = 528
    Top = 422
    Width = 41
    Height = 21
    Caption = 'Exp'
    TabOrder = 31
    OnClick = btnExpClick
  end
  object seExponent: TSpinEdit
    Left = 576
    Top = 422
    Width = 41
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 32
    Value = 2
  end
  object pnlDisplay: TPanel
    Left = 136
    Top = 4
    Width = 177
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
      Left = 80
      Top = 8
      Width = 73
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
    TabOrder = 33
    OnClick = btnGcdClick
  end
  object btnGenWord: TButton
    Left = 72
    Top = 230
    Width = 75
    Height = 21
    Caption = 'Geneate Word'
    TabOrder = 15
    OnClick = btnGenWordClick
  end
  object edtWord: TEdit
    Left = 152
    Top = 230
    Width = 121
    Height = 21
    TabOrder = 16
    Text = '0'
  end
  object btnWordAdd: TButton
    Left = 280
    Top = 230
    Width = 75
    Height = 21
    Caption = 'Add Word'
    TabOrder = 17
    OnClick = btnWordAddClick
  end
  object btnSubWord: TButton
    Left = 360
    Top = 230
    Width = 75
    Height = 21
    Caption = 'Sub Word'
    TabOrder = 18
    OnClick = btnSubWordClick
  end
  object btnMulWord: TButton
    Left = 440
    Top = 230
    Width = 75
    Height = 21
    Caption = 'Mul Word'
    TabOrder = 19
    OnClick = btnMulWordClick
  end
  object btnDivWord: TButton
    Left = 520
    Top = 230
    Width = 75
    Height = 21
    Caption = 'Div Word'
    TabOrder = 20
    OnClick = btnDivWordClick
  end
  object btnModWord: TButton
    Left = 600
    Top = 230
    Width = 75
    Height = 21
    Caption = 'Mod Word'
    TabOrder = 21
    OnClick = btnModWordClick
  end
  object btnVerifyDiv: TButton
    Left = 680
    Top = 230
    Width = 75
    Height = 21
    Caption = 'Verify Div'
    TabOrder = 35
    OnClick = btnVerifyDivClick
  end
end
