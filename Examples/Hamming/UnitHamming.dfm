object FormHamming: TFormHamming
  Left = 192
  Top = 107
  Width = 979
  Height = 563
  Caption = 'Hamming Test'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object btnHamming: TButton
    Left = 48
    Top = 48
    Width = 75
    Height = 25
    Caption = 'Hamming'
    TabOrder = 0
    OnClick = btnHammingClick
  end
  object btnRSTest: TButton
    Left = 192
    Top = 48
    Width = 145
    Height = 25
    Caption = 'Simple RS Test'
    TabOrder = 1
    OnClick = btnRSTestClick
  end
  object btnFiniteField2N: TButton
    Left = 408
    Top = 48
    Width = 153
    Height = 25
    Caption = 'Tes Finite Field 2^N'
    TabOrder = 2
    OnClick = btnFiniteField2NClick
  end
  object btnGenerate2Power8UsingX: TButton
    Left = 616
    Top = 48
    Width = 153
    Height = 25
    Caption = 'Generate 2^8 Using X'
    TabOrder = 3
    OnClick = btnGenerate2Power8UsingXClick
  end
  object btnCalc2Power8: TButton
    Left = 48
    Top = 112
    Width = 75
    Height = 25
    Caption = 'Calc in 2^8'
    TabOrder = 4
    OnClick = btnCalc2Power8Click
  end
  object btnGRSTest: TButton
    Left = 192
    Top = 112
    Width = 145
    Height = 25
    Caption = 'Galios 2^8 RS Test'
    TabOrder = 5
    OnClick = btnGRSTestClick
  end
end
