object FormMath: TFormMath
  Left = 268
  Top = 137
  Width = 932
  Height = 563
  Caption = 'Math Test'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object lblRound: TLabel
    Left = 808
    Top = 56
    Width = 35
    Height = 13
    Caption = 'Round:'
  end
  object edtValue: TEdit
    Left = 16
    Top = 16
    Width = 121
    Height = 21
    TabOrder = 0
    Text = '5'
  end
  object btnInt64Sqrt: TButton
    Left = 152
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Int64 Sqrt'
    TabOrder = 1
    OnClick = btnInt64SqrtClick
  end
  object btnSqrt: TButton
    Left = 240
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Sqrt'
    TabOrder = 2
    OnClick = btnSqrtClick
  end
  object btnLogN: TButton
    Left = 328
    Top = 16
    Width = 75
    Height = 25
    Caption = 'LogN'
    TabOrder = 3
    OnClick = btnLogNClick
  end
  object btnLog2: TButton
    Left = 416
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Log2'
    TabOrder = 4
    OnClick = btnLog2Click
  end
  object btnLog10: TButton
    Left = 504
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Log10'
    TabOrder = 5
    OnClick = btnLog10Click
  end
  object edtResult: TEdit
    Left = 16
    Top = 48
    Width = 649
    Height = 21
    ReadOnly = True
    TabOrder = 6
  end
  object btnFloatPi: TButton
    Left = 592
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Float Pi'
    TabOrder = 7
    OnClick = btnFloatPiClick
  end
  object btnGaussLegendrePi: TButton
    Left = 800
    Top = 16
    Width = 105
    Height = 25
    Caption = 'Gauss Legendre Pi'
    TabOrder = 8
    OnClick = btnGaussLegendrePiClick
  end
  object edtGLRound: TEdit
    Left = 856
    Top = 56
    Width = 33
    Height = 21
    TabOrder = 9
    Text = '8'
  end
  object udGL: TUpDown
    Left = 889
    Top = 56
    Width = 15
    Height = 21
    Associate = edtGLRound
    Min = 0
    Position = 8
    TabOrder = 10
    Wrap = False
  end
  object mmoPi: TMemo
    Left = 16
    Top = 88
    Width = 889
    Height = 241
    ScrollBars = ssVertical
    TabOrder = 11
  end
end
