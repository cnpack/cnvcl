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
        Caption = 'Shamir Threshold Schema'
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
        object btnBNShamirSample2: TButton
          Left = 304
          Top = 24
          Width = 75
          Height = 25
          Caption = 'Sample2'
          TabOrder = 4
          OnClick = btnBNShamirSample2Click
        end
        object btnInt64ShamirSample2: TButton
          Left = 104
          Top = 24
          Width = 75
          Height = 25
          Caption = 'Sample2'
          TabOrder = 5
          OnClick = btnInt64ShamirSample2Click
        end
      end
    end
    object tsFeldmanVSS: TTabSheet
      Caption = 'Feldman VSS'
      ImageIndex = 1
      object grpFeldman: TGroupBox
        Left = 8
        Top = 8
        Width = 929
        Height = 473
        Caption = 'Feldman VSS'
        TabOrder = 0
        object btnFeldmanGen: TButton
          Left = 832
          Top = 24
          Width = 75
          Height = 25
          Caption = 'Gen Key'
          TabOrder = 0
          OnClick = btnFeldmanGenClick
        end
        object btnInt64FeldmanCheckParam: TButton
          Left = 456
          Top = 24
          Width = 129
          Height = 25
          Caption = 'Feldman Check Param'
          TabOrder = 1
          OnClick = btnInt64FeldmanCheckParamClick
        end
        object btnFeldmanCheckParam2: TButton
          Left = 600
          Top = 24
          Width = 129
          Height = 25
          Caption = 'Feldman Check Param2'
          TabOrder = 2
          OnClick = btnFeldmanCheckParam2Click
        end
        object btnInt64FeldmanGen: TButton
          Left = 744
          Top = 24
          Width = 75
          Height = 25
          Caption = 'Int64 Gen Key'
          TabOrder = 3
          OnClick = btnInt64FeldmanGenClick
        end
        object btnInt64Feldman: TButton
          Left = 16
          Top = 24
          Width = 75
          Height = 25
          Caption = 'Sample'
          TabOrder = 4
          OnClick = btnInt64FeldmanClick
        end
        object mmoInt64Feldman: TMemo
          Left = 16
          Top = 64
          Width = 249
          Height = 393
          TabOrder = 5
        end
        object btnFeldmanSample: TButton
          Left = 280
          Top = 24
          Width = 75
          Height = 25
          Caption = 'Sample'
          TabOrder = 6
          OnClick = btnFeldmanSampleClick
        end
        object mmoBNFeldman: TMemo
          Left = 280
          Top = 64
          Width = 625
          Height = 393
          TabOrder = 7
        end
      end
    end
  end
end
