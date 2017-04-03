object FormRSA: TFormRSA
  Left = 259
  Top = 72
  BorderStyle = bsDialog
  Caption = 'RSA Demo'
  ClientHeight = 476
  ClientWidth = 703
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object pgc1: TPageControl
    Left = 16
    Top = 16
    Width = 665
    Height = 441
    ActivePage = tsInt64RSA
    TabOrder = 0
    object tsInt64RSA: TTabSheet
      Caption = 'Int64 RSA'
      object grpKeys: TGroupBox
        Left = 16
        Top = 16
        Width = 617
        Height = 233
        Caption = 'Keys:'
        TabOrder = 0
        object lblPrime1: TLabel
          Left = 16
          Top = 24
          Width = 35
          Height = 13
          Caption = 'Prime1:'
        end
        object lblPrime2: TLabel
          Left = 352
          Top = 24
          Width = 35
          Height = 13
          Caption = 'Prime2:'
        end
        object lblPrivProduct: TLabel
          Left = 16
          Top = 56
          Width = 89
          Height = 13
          Caption = 'Private Pruduct (*):'
        end
        object lblPrivExp: TLabel
          Left = 16
          Top = 88
          Width = 99
          Height = 13
          Caption = 'Private Exponent (d):'
        end
        object lblPubProduct: TLabel
          Left = 16
          Top = 128
          Width = 85
          Height = 13
          Caption = 'Public Pruduct (*):'
        end
        object lblPubExp: TLabel
          Left = 16
          Top = 160
          Width = 95
          Height = 13
          Caption = 'Public Exponent (e):'
        end
        object edtPrime1: TEdit
          Left = 128
          Top = 20
          Width = 201
          Height = 21
          ReadOnly = True
          TabOrder = 0
        end
        object edtPrime2: TEdit
          Left = 400
          Top = 20
          Width = 201
          Height = 21
          ReadOnly = True
          TabOrder = 1
        end
        object edtPrivProduct: TEdit
          Left = 128
          Top = 52
          Width = 473
          Height = 21
          ReadOnly = True
          TabOrder = 2
        end
        object edtPrivExp: TEdit
          Left = 128
          Top = 84
          Width = 473
          Height = 21
          ReadOnly = True
          TabOrder = 3
        end
        object edtPubProduct: TEdit
          Left = 128
          Top = 124
          Width = 473
          Height = 21
          ReadOnly = True
          TabOrder = 4
        end
        object edtPubExp: TEdit
          Left = 128
          Top = 156
          Width = 473
          Height = 21
          ReadOnly = True
          TabOrder = 5
        end
        object btnGenerateRSA: TButton
          Left = 128
          Top = 192
          Width = 281
          Height = 25
          Caption = 'Generate RSA Keys'
          TabOrder = 6
          OnClick = btnGenerateRSAClick
        end
      end
      object grpCrypt: TGroupBox
        Left = 16
        Top = 264
        Width = 617
        Height = 121
        Caption = 'Crypt'
        TabOrder = 1
        object lblData: TLabel
          Left = 16
          Top = 24
          Width = 36
          Height = 13
          Caption = 'Integer:'
        end
        object lblRes: TLabel
          Left = 16
          Top = 56
          Width = 33
          Height = 13
          Caption = 'Result:'
        end
        object lblDataBack: TLabel
          Left = 16
          Top = 88
          Width = 36
          Height = 13
          Caption = 'Integer:'
        end
        object edtData: TEdit
          Left = 64
          Top = 20
          Width = 425
          Height = 21
          TabOrder = 0
          Text = '12345678987654321'
        end
        object edtRes: TEdit
          Left = 64
          Top = 52
          Width = 425
          Height = 21
          TabOrder = 1
        end
        object edtDataBack: TEdit
          Left = 64
          Top = 84
          Width = 425
          Height = 21
          ReadOnly = True
          TabOrder = 2
        end
        object btnRSAEn: TButton
          Left = 504
          Top = 34
          Width = 89
          Height = 25
          Caption = 'RSA Encrypt'
          TabOrder = 3
          OnClick = btnRSAEnClick
        end
        object btnRSADe: TButton
          Left = 504
          Top = 66
          Width = 89
          Height = 25
          Caption = 'RSA Decrypt'
          TabOrder = 4
          OnClick = btnRSADeClick
        end
      end
    end
  end
end
