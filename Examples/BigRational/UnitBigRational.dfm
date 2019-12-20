object FormRational: TFormRational
  Left = 192
  Top = 107
  Width = 1142
  Height = 656
  Caption = 'Big Rational Number Test'
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
  object lblFloat: TLabel
    Left = 16
    Top = 184
    Width = 48
    Height = 13
    Caption = 'Extended:'
  end
  object grpBR1: TGroupBox
    Left = 16
    Top = 16
    Width = 465
    Height = 145
    Caption = 'Big Rational Number 1:'
    TabOrder = 0
    object bvl1: TBevel
      Left = 24
      Top = 48
      Width = 273
      Height = 18
      Shape = bsBottomLine
    end
    object edtBR1N: TEdit
      Left = 24
      Top = 24
      Width = 273
      Height = 21
      TabOrder = 0
      Text = '10000'
    end
    object edtBR1D: TEdit
      Left = 24
      Top = 88
      Width = 273
      Height = 21
      TabOrder = 1
      Text = '200'
    end
    object btnSet1: TButton
      Left = 312
      Top = 24
      Width = 75
      Height = 21
      Caption = 'Set String'
      TabOrder = 2
      OnClick = btnSet1Click
    end
    object btnSet2: TButton
      Left = 312
      Top = 56
      Width = 75
      Height = 21
      Caption = 'Set Value'
      TabOrder = 3
      OnClick = btnSet2Click
    end
    object btnSet3: TButton
      Left = 312
      Top = 88
      Width = 75
      Height = 21
      Caption = 'Set Int'
      TabOrder = 4
      OnClick = btnSet3Click
    end
    object btnReduce: TButton
      Left = 400
      Top = 24
      Width = 57
      Height = 84
      Caption = 'Reduce'
      TabOrder = 5
      OnClick = btnReduceClick
    end
  end
  object grpRN2: TGroupBox
    Left = 624
    Top = 16
    Width = 433
    Height = 145
    Caption = 'Big Rational Number 2:'
    TabOrder = 1
    object bvlRN2: TBevel
      Left = 24
      Top = 48
      Width = 273
      Height = 18
      Shape = bsBottomLine
    end
    object edtBR2N: TEdit
      Left = 24
      Top = 24
      Width = 273
      Height = 21
      TabOrder = 0
      Text = '20000'
    end
    object edtBR2D: TEdit
      Left = 24
      Top = 88
      Width = 273
      Height = 21
      TabOrder = 1
      Text = '250'
    end
    object btnRN2SetValue: TButton
      Left = 312
      Top = 56
      Width = 75
      Height = 21
      Caption = 'Set Value'
      TabOrder = 2
      OnClick = btnRN2SetValueClick
    end
  end
  object btnAdd: TButton
    Left = 536
    Top = 24
    Width = 25
    Height = 25
    Caption = '+'
    TabOrder = 2
    OnClick = btnAddClick
  end
  object btnSub: TButton
    Left = 536
    Top = 61
    Width = 25
    Height = 25
    Caption = '-'
    TabOrder = 3
    OnClick = btnSubClick
  end
  object btnMul: TButton
    Left = 536
    Top = 99
    Width = 25
    Height = 25
    Caption = '*'
    TabOrder = 4
    OnClick = btnMulClick
  end
  object btnDiv: TButton
    Left = 536
    Top = 136
    Width = 25
    Height = 25
    Caption = '/'
    TabOrder = 5
    OnClick = btnDivClick
  end
  object edtExtended: TEdit
    Left = 80
    Top = 184
    Width = 233
    Height = 21
    TabOrder = 6
    Text = '-123321.847820398924'
  end
  object btnSetExtended: TButton
    Left = 336
    Top = 184
    Width = 75
    Height = 21
    Caption = 'Set Extended'
    TabOrder = 7
    OnClick = btnSetExtendedClick
  end
  object btnSetString: TButton
    Left = 424
    Top = 184
    Width = 75
    Height = 21
    Caption = 'Set String'
    TabOrder = 8
    OnClick = btnSetStringClick
  end
end
