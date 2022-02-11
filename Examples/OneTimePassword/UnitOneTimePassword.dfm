object FormOneTimePassword: TFormOneTimePassword
  Left = 192
  Top = 107
  Width = 979
  Height = 563
  Caption = 'One-Time-Password Test'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object btnGenerate: TButton
    Left = 24
    Top = 24
    Width = 75
    Height = 25
    Caption = 'Generate'
    TabOrder = 0
    OnClick = btnGenerateClick
  end
end
