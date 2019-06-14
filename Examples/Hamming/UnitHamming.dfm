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
end
