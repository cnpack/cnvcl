object FormSM2: TFormSM2
  Left = 277
  Top = 167
  BorderStyle = bsDialog
  Caption = 'SM2 Test'
  ClientHeight = 561
  ClientWidth = 959
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
  object lblSM2PublicKey: TLabel
    Left = 16
    Top = 20
    Width = 109
    Height = 13
    Caption = 'SM2 Public Key (Hex)::'
  end
  object lblSM2PrivateKey: TLabel
    Left = 16
    Top = 52
    Width = 110
    Height = 13
    Caption = 'SM2 Private Key (Hex):'
  end
  object pgcSm2: TPageControl
    Left = 16
    Top = 88
    Width = 921
    Height = 449
    ActivePage = tsEncDec
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object tsEncDec: TTabSheet
      Caption = 'Encryttion / Decryption'
      object grpSm2Enc: TGroupBox
        Left = 8
        Top = 8
        Width = 889
        Height = 473
        Caption = 'Encryption / Decryption'
        TabOrder = 0
        object bvl1: TBevel
          Left = 16
          Top = 80
          Width = 857
          Height = 9
          Shape = bsTopLine
        end
        object bvl2: TBevel
          Left = 152
          Top = 16
          Width = 17
          Height = 50
          Shape = bsLeftLine
        end
        object lblSM2Text: TLabel
          Left = 168
          Top = 36
          Width = 79
          Height = 13
          Caption = 'Text To Encrypt:'
        end
        object btnSm2Example1: TButton
          Left = 16
          Top = 32
          Width = 113
          Height = 25
          Caption = 'Sm2 Example 192'
          TabOrder = 0
          OnClick = btnSm2Example1Click
        end
        object edtSM2Text: TEdit
          Left = 256
          Top = 32
          Width = 481
          Height = 21
          TabOrder = 1
          Text = '123456'
        end
        object btnSM2Encrypt: TButton
          Left = 744
          Top = 32
          Width = 129
          Height = 25
          Caption = 'Encrypt using Public Key'
          TabOrder = 2
          OnClick = btnSM2EncryptClick
        end
        object mmoSM2Results: TMemo
          Left = 16
          Top = 104
          Width = 857
          Height = 113
          TabOrder = 3
        end
        object btnSM2Decrypt: TButton
          Left = 744
          Top = 232
          Width = 129
          Height = 25
          Caption = 'Decrypt using Private Key'
          TabOrder = 4
          OnClick = btnSM2DecryptClick
        end
      end
    end
    object tsSignVerify: TTabSheet
      Caption = 'Sign / Verify'
      ImageIndex = 1
      object grpSm2SignVerify: TGroupBox
        Left = 8
        Top = 8
        Width = 889
        Height = 473
        Caption = 'Encryption / Decryption'
        TabOrder = 0
        object btnSm2SignVerify: TButton
          Left = 16
          Top = 32
          Width = 113
          Height = 25
          Caption = 'Sm2 Example 256'
          TabOrder = 0
          OnClick = btnSm2SignVerifyClick
        end
      end
    end
    object tsKeyExchange: TTabSheet
      Caption = 'Key Exchange'
      ImageIndex = 2
      object grpSM2KeyExchange: TGroupBox
        Left = 8
        Top = 8
        Width = 889
        Height = 473
        Caption = 'Encryption / Decryption'
        TabOrder = 0
        object btnSM2KeyExchange: TButton
          Left = 16
          Top = 32
          Width = 113
          Height = 25
          Caption = 'Sm2 Example 256'
          TabOrder = 0
          OnClick = btnSM2KeyExchangeClick
        end
      end
    end
  end
  object edtSM2PublicKey: TEdit
    Left = 136
    Top = 16
    Width = 673
    Height = 21
    TabOrder = 1
    Text = 
      '047D6F2CD515E5C1B76DF111EBC3DF4439970542A4F3421C3293E28B2ECFC8E7' +
      'A73B013D099A36C74C45B8338550A51FF60BA690493DD3DDE76005605FF83D77' +
      '83'
  end
  object edtSM2PrivateKey: TEdit
    Left = 136
    Top = 48
    Width = 673
    Height = 21
    TabOrder = 2
    Text = '9F471257D828D604B7426EFA1D1D58B855771FB2DA34F7E57FA35D1BDE5E6EB7'
  end
  object btnGenerateKey: TButton
    Left = 824
    Top = 16
    Width = 105
    Height = 25
    Caption = 'Generate Key'
    TabOrder = 3
    OnClick = btnGenerateKeyClick
  end
end
