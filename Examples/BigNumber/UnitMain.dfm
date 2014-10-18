object FormBigNumber: TFormBigNumber
  Left = 160
  Top = 30
  Width = 920
  Height = 679
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
    Top = 224
    Width = 46
    Height = 13
    Caption = 'Number 2'
  end
  object lblBytes: TLabel
    Left = 344
    Top = 16
    Width = 55
    Height = 13
    Caption = 'Byte Count:'
  end
  object mmoNum1: TMemo
    Left = 16
    Top = 40
    Width = 873
    Height = 169
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object mmoNum2: TMemo
    Left = 16
    Top = 248
    Width = 873
    Height = 169
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object btnGen1: TButton
    Left = 816
    Top = 12
    Width = 75
    Height = 21
    Caption = 'Generate'
    TabOrder = 2
    OnClick = btnGen1Click
  end
  object btnGen2: TButton
    Left = 816
    Top = 220
    Width = 75
    Height = 21
    Caption = 'Generate'
    TabOrder = 3
    OnClick = btnGen2Click
  end
  object mmoResult: TMemo
    Left = 16
    Top = 448
    Width = 873
    Height = 169
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 4
  end
  object btnDup: TButton
    Left = 120
    Top = 220
    Width = 75
    Height = 21
    Caption = 'Copy'
    TabOrder = 5
    OnClick = btnDupClick
  end
  object btnSwap: TButton
    Left = 240
    Top = 220
    Width = 75
    Height = 21
    Caption = 'Swap'
    TabOrder = 6
    OnClick = btnSwapClick
  end
  object btnCompare: TButton
    Left = 352
    Top = 220
    Width = 75
    Height = 21
    Caption = 'Compare'
    TabOrder = 7
    OnClick = btnCompareClick
  end
  object btnInverseNeg1: TButton
    Left = 688
    Top = 12
    Width = 99
    Height = 21
    Caption = 'Inverse Negative'
    TabOrder = 8
    OnClick = btnInverseNeg1Click
  end
  object btnInverseNeg2: TButton
    Left = 688
    Top = 220
    Width = 99
    Height = 21
    Caption = 'Inverse Negative'
    TabOrder = 9
    OnClick = btnInverseNeg2Click
  end
  object cbbDigits: TComboBox
    Left = 404
    Top = 12
    Width = 145
    Height = 21
    ItemHeight = 13
    TabOrder = 10
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
    TabOrder = 11
    OnClick = btnUAddClick
  end
  object btnUsub: TButton
    Left = 104
    Top = 422
    Width = 75
    Height = 21
    Caption = 'Unsigned Sub'
    TabOrder = 12
    OnClick = btnUsubClick
  end
  object btnSignedAdd: TButton
    Left = 184
    Top = 422
    Width = 75
    Height = 21
    Caption = 'Signed Add'
    TabOrder = 13
    OnClick = btnSignedAddClick
  end
  object btnSignedSub: TButton
    Left = 264
    Top = 422
    Width = 75
    Height = 21
    Caption = 'Signed Sub'
    TabOrder = 14
    OnClick = btnSignedSubClick
  end
  object btnShiftleftOne: TButton
    Left = 344
    Top = 422
    Width = 75
    Height = 21
    Caption = 'Shift Left 1'
    TabOrder = 15
    OnClick = btnShiftleftOneClick
  end
  object btnShiftRightOne: TButton
    Left = 432
    Top = 422
    Width = 75
    Height = 21
    Caption = 'Shift Right 1'
    TabOrder = 16
    OnClick = btnShiftRightOneClick
  end
end
