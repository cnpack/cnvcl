object FormSecretSharing: TFormSecretSharing
  Left = 192
  Top = 107
  Width = 979
  Height = 563
  Caption = 'Test SecretSharing'
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
  object pgc1: TPageControl
    Left = 8
    Top = 8
    Width = 953
    Height = 518
    ActivePage = tsInt64Shamir
    TabOrder = 0
    object tsInt64Shamir: TTabSheet
      Caption = 'Shamir Threshold Schema'
      object grpInt64Shamir: TGroupBox
        Left = 8
        Top = 8
        Width = 929
        Height = 473
        Caption = 'Int64 Shamir Threshold Schema'
        TabOrder = 0
        object btnInt64ShamirSample: TButton
          Left = 16
          Top = 24
          Width = 75
          Height = 25
          Caption = 'Sample'
          TabOrder = 0
          OnClick = btnInt64ShamirSampleClick
        end
        object mmoInt64Shamir: TMemo
          Left = 16
          Top = 64
          Width = 161
          Height = 393
          TabOrder = 1
        end
        object btnBNShamirSample: TButton
          Left = 216
          Top = 24
          Width = 75
          Height = 25
          Caption = 'Sample'
          TabOrder = 2
          OnClick = btnBNShamirSampleClick
        end
        object mmoBNShamir: TMemo
          Left = 216
          Top = 64
          Width = 697
          Height = 393
          TabOrder = 3
        end
      end
    end
  end
end
