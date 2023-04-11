object FormOneTimePassword: TFormOneTimePassword
  Left = 192
  Top = 108
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
    Width = 225
    Height = 25
    Caption = 'Generate GB/T 38556-2020'
    TabOrder = 0
    OnClick = btnGenerateClick
  end
  object btnGen2: TButton
    Left = 24
    Top = 72
    Width = 225
    Height = 25
    Caption = 'Generate HOTP rfc4226'
    TabOrder = 1
    OnClick = btnGen2Click
  end
  object btnGen3: TButton
    Left = 24
    Top = 120
    Width = 225
    Height = 25
    Caption = 'Generate TOTP rfc6238'
    TabOrder = 2
    OnClick = btnGen3Click
  end
end
