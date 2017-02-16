object FormPrime: TFormPrime
  Left = 210
  Top = 99
  BorderStyle = bsDialog
  Caption = 'Prime Number Test'
  ClientHeight = 464
  ClientWidth = 687
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
    Left = 16
    Top = 16
    Width = 657
    Height = 429
    ActivePage = tsGenPrime
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object tsGenPrime: TTabSheet
      Caption = 'Find Prime Numbers'
      object btnGen: TButton
        Left = 16
        Top = 16
        Width = 209
        Height = 25
        Caption = 'Find Prime Numbers from 2 to'
        TabOrder = 0
        OnClick = btnGenClick
      end
      object edtMax: TEdit
        Left = 248
        Top = 16
        Width = 121
        Height = 21
        TabOrder = 1
        Text = '65537'
      end
      object mmoResult: TMemo
        Left = 16
        Top = 56
        Width = 617
        Height = 325
        Anchors = [akLeft, akTop, akRight, akBottom]
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 2
      end
      object chkQuickGen: TCheckBox
        Left = 392
        Top = 16
        Width = 97
        Height = 17
        Caption = 'Quick Mode'
        Checked = True
        State = cbChecked
        TabOrder = 3
      end
    end
    object tsIsPrime: TTabSheet
      Caption = 'Is Prime'
      ImageIndex = 1
      object lblCheck: TLabel
        Left = 16
        Top = 28
        Width = 31
        Height = 13
        Caption = 'Check'
      end
      object lblInt64: TLabel
        Left = 16
        Top = 68
        Width = 31
        Height = 13
        Caption = 'Check'
      end
      object edtToPrime: TEdit
        Left = 64
        Top = 24
        Width = 121
        Height = 21
        TabOrder = 0
        Text = '39779'
      end
      object btnIsPrime: TButton
        Left = 208
        Top = 24
        Width = 75
        Height = 25
        Caption = 'Is Prime£¿'
        TabOrder = 1
        OnClick = btnIsPrimeClick
      end
      object edtInt64: TEdit
        Left = 64
        Top = 64
        Width = 121
        Height = 21
        TabOrder = 2
        Text = '397796406237767'
      end
      object btnInt64IsPrime: TButton
        Left = 208
        Top = 64
        Width = 105
        Height = 25
        Caption = 'Is Int64 Prime£¿'
        TabOrder = 3
        OnClick = btnInt64IsPrimeClick
      end
      object btnCarmichael: TButton
        Left = 336
        Top = 64
        Width = 297
        Height = 25
        Caption = 'Carmichael is Prime?'
        TabOrder = 4
        OnClick = btnCarmichaelClick
      end
      object mmoCar: TMemo
        Left = 16
        Top = 112
        Width = 617
        Height = 269
        Anchors = [akLeft, akTop, akRight, akBottom]
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 5
      end
      object btnGen64: TButton
        Left = 320
        Top = 24
        Width = 145
        Height = 25
        Caption = 'Generate a Int64 Prime'
        TabOrder = 6
        OnClick = btnGen64Click
      end
    end
    object tsSimpleRSA: TTabSheet
      Caption = 'SimpleRSA'
      ImageIndex = 2
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
