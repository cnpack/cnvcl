object FormRSA: TFormRSA
  Left = 180
  Top = 45
  BorderStyle = bsDialog
  Caption = 'RSA Demo'
  ClientHeight = 542
  ClientWidth = 851
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object pgc1: TPageControl
    Left = 16
    Top = 16
    Width = 817
    Height = 513
    ActivePage = tsEuclid
    TabOrder = 0
    object tsInt64RSA: TTabSheet
      Caption = 'Int64 RSA'
      object grpKeys: TGroupBox
        Left = 16
        Top = 16
        Width = 777
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
          Left = 424
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
          Caption = 'Private Product (*):'
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
          Caption = 'Public Product (*):'
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
          Width = 273
          Height = 21
          ReadOnly = True
          TabOrder = 0
        end
        object edtPrime2: TEdit
          Left = 480
          Top = 20
          Width = 281
          Height = 21
          ReadOnly = True
          TabOrder = 1
        end
        object edtPrivProduct: TEdit
          Left = 128
          Top = 52
          Width = 633
          Height = 21
          ReadOnly = True
          TabOrder = 2
        end
        object edtPrivExp: TEdit
          Left = 128
          Top = 84
          Width = 633
          Height = 21
          ReadOnly = True
          TabOrder = 3
        end
        object edtPubProduct: TEdit
          Left = 128
          Top = 124
          Width = 633
          Height = 21
          ReadOnly = True
          TabOrder = 4
        end
        object edtPubExp: TEdit
          Left = 128
          Top = 156
          Width = 633
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
        Top = 344
        Width = 777
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
          Width = 577
          Height = 21
          TabOrder = 0
          Text = '12345678987654321'
        end
        object edtRes: TEdit
          Left = 64
          Top = 52
          Width = 577
          Height = 21
          TabOrder = 2
        end
        object edtDataBack: TEdit
          Left = 64
          Top = 84
          Width = 577
          Height = 21
          ReadOnly = True
          TabOrder = 4
        end
        object btnRSAEn: TButton
          Left = 664
          Top = 34
          Width = 89
          Height = 25
          Caption = 'RSA Encrypt'
          TabOrder = 1
          OnClick = btnRSAEnClick
        end
        object btnRSADe: TButton
          Left = 664
          Top = 66
          Width = 89
          Height = 25
          Caption = 'RSA Decrypt'
          TabOrder = 3
          OnClick = btnRSADeClick
        end
      end
    end
    object tsRSA: TTabSheet
      Caption = 'Big Number RSA'
      ImageIndex = 1
      object grpBNKeys: TGroupBox
        Left = 16
        Top = 16
        Width = 777
        Height = 321
        Caption = 'Big Number Keys'
        TabOrder = 0
        object lblBNP1: TLabel
          Left = 16
          Top = 24
          Width = 35
          Height = 13
          Caption = 'Prime1:'
        end
        object lblBNP2: TLabel
          Left = 16
          Top = 56
          Width = 35
          Height = 13
          Caption = 'Prime2:'
        end
        object lblBNPrivProduct: TLabel
          Left = 16
          Top = 88
          Width = 89
          Height = 13
          Caption = 'Private Product (*):'
        end
        object lblBNPrivExp: TLabel
          Left = 16
          Top = 152
          Width = 99
          Height = 13
          Caption = 'Private Exponent (d):'
        end
        object lblBNPubProduct: TLabel
          Left = 16
          Top = 184
          Width = 85
          Height = 13
          Caption = 'Public Product (*):'
        end
        object lblBNPubExp: TLabel
          Left = 16
          Top = 248
          Width = 95
          Height = 13
          Caption = 'Public Exponent (e):'
        end
        object lblBits: TLabel
          Left = 576
          Top = 288
          Width = 20
          Height = 13
          Caption = 'Bits:'
        end
        object edtBNPrime1: TEdit
          Left = 128
          Top = 20
          Width = 633
          Height = 21
          ReadOnly = True
          TabOrder = 0
        end
        object edtBNPrime2: TEdit
          Left = 128
          Top = 52
          Width = 633
          Height = 21
          ReadOnly = True
          TabOrder = 1
        end
        object btnBNGen: TButton
          Left = 128
          Top = 280
          Width = 281
          Height = 25
          Caption = 'Generate RSA Keys'
          TabOrder = 6
          OnClick = btnBNGenClick
        end
        object edtBNPrivExp: TEdit
          Left = 128
          Top = 148
          Width = 633
          Height = 21
          ReadOnly = True
          TabOrder = 3
        end
        object edtBNPubExp: TEdit
          Left = 128
          Top = 244
          Width = 633
          Height = 21
          ReadOnly = True
          TabOrder = 5
        end
        object mmoBNPrivProduct: TMemo
          Left = 128
          Top = 88
          Width = 633
          Height = 45
          ScrollBars = ssVertical
          TabOrder = 2
        end
        object mmoBNPubProduct: TMemo
          Left = 128
          Top = 184
          Width = 633
          Height = 45
          ScrollBars = ssVertical
          TabOrder = 4
        end
        object cbbBits: TComboBox
          Left = 616
          Top = 284
          Width = 145
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 7
          Items.Strings = (
            '4096'
            '2048'
            '1024'
            '512'
            '256'
            '128'
            '64')
        end
      end
      object grpBNCrypt: TGroupBox
        Left = 16
        Top = 344
        Width = 777
        Height = 121
        Caption = 'Crypt'
        TabOrder = 1
        object lblBNInteger: TLabel
          Left = 16
          Top = 24
          Width = 41
          Height = 13
          Caption = 'Decimal:'
        end
        object lblBNResult: TLabel
          Left = 16
          Top = 56
          Width = 33
          Height = 13
          Caption = 'Result:'
        end
        object lblBNDecrypt: TLabel
          Left = 16
          Top = 88
          Width = 41
          Height = 13
          Caption = 'Decimal:'
        end
        object edtBNData: TEdit
          Left = 64
          Top = 20
          Width = 577
          Height = 21
          TabOrder = 0
          Text = '1234567890987654321'
        end
        object edtBNRes: TEdit
          Left = 64
          Top = 52
          Width = 577
          Height = 21
          TabOrder = 2
        end
        object edtBNDataBack: TEdit
          Left = 64
          Top = 84
          Width = 577
          Height = 21
          ReadOnly = True
          TabOrder = 4
        end
        object btnBNRSAEn: TButton
          Left = 664
          Top = 34
          Width = 89
          Height = 25
          Caption = 'RSA Encrypt'
          TabOrder = 1
          OnClick = btnBNRSAEnClick
        end
        object btnBNRSADe: TButton
          Left = 664
          Top = 66
          Width = 89
          Height = 25
          Caption = 'RSA Decrypt'
          TabOrder = 3
          OnClick = btnBNRSADeClick
        end
      end
    end
    object tsEuclid: TTabSheet
      Caption = 'Extended Euclidean Gcd'
      ImageIndex = 2
      object grpEuclidean: TGroupBox
        Left = 16
        Top = 16
        Width = 777
        Height = 401
        Caption = 'grpEuclidean'
        TabOrder = 0
        object lblEqual: TLabel
          Left = 32
          Top = 24
          Width = 423
          Height = 13
          Caption = 
            'A     *     X                                  +                ' +
            '               B     *     Y                   =                ' +
            '   1'
        end
        object lblA: TLabel
          Left = 32
          Top = 56
          Width = 10
          Height = 13
          Caption = 'A:'
        end
        object lblB: TLabel
          Left = 280
          Top = 56
          Width = 10
          Height = 13
          Caption = 'B:'
        end
        object lblX: TLabel
          Left = 32
          Top = 136
          Width = 10
          Height = 13
          Caption = 'X:'
        end
        object lblY: TLabel
          Left = 280
          Top = 136
          Width = 10
          Height = 13
          Caption = 'Y:'
        end
        object lblX0: TLabel
          Left = 56
          Top = 168
          Width = 10
          Height = 13
          Caption = 'X:'
        end
        object lblXP: TLabel
          Left = 32
          Top = 200
          Width = 12
          Height = 13
          Caption = 'X'#39':'
        end
        object edtA: TEdit
          Left = 56
          Top = 52
          Width = 193
          Height = 21
          TabOrder = 0
          Text = '65537'
        end
        object edtB: TEdit
          Left = 320
          Top = 52
          Width = 193
          Height = 21
          TabOrder = 1
          Text = '991958164832267712'
        end
        object edtX: TEdit
          Left = 56
          Top = 132
          Width = 193
          Height = 21
          ReadOnly = True
          TabOrder = 4
        end
        object edtY: TEdit
          Left = 320
          Top = 132
          Width = 193
          Height = 21
          ReadOnly = True
          TabOrder = 5
        end
        object btnInt64Euc: TButton
          Left = 56
          Top = 88
          Width = 193
          Height = 25
          Caption = 'Int64 Extended Euclidean Gcd'
          Enabled = False
          TabOrder = 2
          OnClick = btnInt64EucClick
        end
        object btnBNGcd: TButton
          Left = 320
          Top = 88
          Width = 193
          Height = 25
          Caption = 'BigNumber Extended Euclidean Gcd'
          TabOrder = 3
          OnClick = btnBNGcdClick
        end
        object edtXP: TEdit
          Left = 56
          Top = 196
          Width = 193
          Height = 21
          ReadOnly = True
          TabOrder = 6
        end
      end
    end
  end
end
