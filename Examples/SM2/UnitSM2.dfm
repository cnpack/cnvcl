object FormSM2: TFormSM2
  Left = 354
  Top = 220
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
  object pgcSm2: TPageControl
    Left = 16
    Top = 16
    Width = 921
    Height = 521
    ActivePage = tsKeyExchange
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
        object btnSm2Example1: TButton
          Left = 16
          Top = 32
          Width = 113
          Height = 25
          Caption = 'Sm2 Example 192'
          TabOrder = 0
          OnClick = btnSm2Example1Click
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
end
