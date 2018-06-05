object FormRSA: TFormRSA
  Left = 115
  Top = 97
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
    ActivePage = tsInt64RSA
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
          Width = 50
          Height = 13
          Caption = 'Prime1 (p):'
        end
        object lblPrime2: TLabel
          Left = 424
          Top = 24
          Width = 50
          Height = 13
          Caption = 'Prime2 (q):'
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
        object lblInt64MBits: TLabel
          Left = 16
          Top = 196
          Width = 29
          Height = 13
          Caption = 'n Bits:'
        end
        object edtPrime1: TEdit
          Left = 128
          Top = 20
          Width = 273
          Height = 21
          ParentShowHint = False
          ReadOnly = True
          ShowHint = True
          TabOrder = 0
          OnChange = edtBNChange
        end
        object edtPrime2: TEdit
          Left = 480
          Top = 20
          Width = 281
          Height = 21
          ParentShowHint = False
          ReadOnly = True
          ShowHint = True
          TabOrder = 1
          OnChange = edtBNChange
        end
        object edtPrivProduct: TEdit
          Left = 128
          Top = 52
          Width = 633
          Height = 21
          ParentShowHint = False
          ReadOnly = True
          ShowHint = True
          TabOrder = 2
          OnChange = edtBNChange
        end
        object edtPrivExp: TEdit
          Left = 128
          Top = 84
          Width = 633
          Height = 21
          ParentShowHint = False
          ReadOnly = True
          ShowHint = True
          TabOrder = 3
          OnChange = edtBNChange
        end
        object edtPubProduct: TEdit
          Left = 128
          Top = 124
          Width = 633
          Height = 21
          ParentShowHint = False
          ReadOnly = True
          ShowHint = True
          TabOrder = 4
          OnChange = edtBNChange
        end
        object edtPubExp: TEdit
          Left = 128
          Top = 156
          Width = 633
          Height = 21
          ParentShowHint = False
          ReadOnly = True
          ShowHint = True
          TabOrder = 5
          OnChange = edtBNChange
        end
        object btnGenerateRSA: TButton
          Left = 128
          Top = 192
          Width = 281
          Height = 21
          Caption = 'Generate RSA Keys'
          TabOrder = 6
          OnClick = btnGenerateRSAClick
        end
        object btnSendR: TButton
          Left = 584
          Top = 192
          Width = 177
          Height = 21
          Caption = 'Send (p-1)*(q-1) to Euclidean B'
          TabOrder = 7
          OnClick = btnSendRClick
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
          Height = 21
          Caption = 'RSA Encrypt'
          TabOrder = 1
          OnClick = btnRSAEnClick
        end
        object btnRSADe: TButton
          Left = 664
          Top = 66
          Width = 89
          Height = 21
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
          Width = 50
          Height = 13
          Caption = 'Prime1 (p):'
        end
        object lblBNP2: TLabel
          Left = 16
          Top = 56
          Width = 50
          Height = 13
          Caption = 'Prime2 (q):'
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
        object lblPBits: TLabel
          Left = 384
          Top = 284
          Width = 49
          Height = 13
          Caption = 'Prime Bits:'
        end
        object lblSaveFormat: TLabel
          Left = 656
          Top = 284
          Width = 35
          Height = 13
          Caption = 'Format:'
        end
        object Bevel1: TBevel
          Left = 504
          Top = 280
          Width = 9
          Height = 25
          Shape = bsLeftLine
        end
        object lblModulusBits: TLabel
          Left = 16
          Top = 116
          Width = 29
          Height = 13
          Caption = 'n Bits:'
        end
        object bvl1: TBevel
          Left = 264
          Top = 280
          Width = 9
          Height = 25
          Shape = bsLeftLine
        end
        object lblMBits: TLabel
          Left = 128
          Top = 284
          Width = 63
          Height = 13
          Caption = 'Modulus Bits:'
        end
        object edtBNPrime1: TEdit
          Left = 128
          Top = 20
          Width = 561
          Height = 21
          ParentShowHint = False
          ReadOnly = True
          ShowHint = True
          TabOrder = 0
          OnChange = edtBNChange
        end
        object edtBNPrime2: TEdit
          Left = 128
          Top = 52
          Width = 561
          Height = 21
          ParentShowHint = False
          ReadOnly = True
          ShowHint = True
          TabOrder = 3
          OnChange = edtBNChange
        end
        object btnBNGen: TButton
          Left = 272
          Top = 280
          Width = 105
          Height = 21
          Caption = 'Generate RSA Keys'
          TabOrder = 12
          OnClick = btnBNGenClick
        end
        object edtBNPrivExp: TEdit
          Left = 128
          Top = 148
          Width = 633
          Height = 21
          ParentShowHint = False
          ReadOnly = True
          ShowHint = True
          TabOrder = 5
          OnChange = edtBNChange
        end
        object edtBNPubExp: TEdit
          Left = 128
          Top = 244
          Width = 569
          Height = 21
          ParentShowHint = False
          ReadOnly = True
          ShowHint = True
          TabOrder = 8
          OnChange = edtBNChange
        end
        object mmoBNPrivProduct: TMemo
          Left = 128
          Top = 88
          Width = 633
          Height = 45
          ParentShowHint = False
          ScrollBars = ssVertical
          ShowHint = True
          TabOrder = 4
          OnChange = mmoBNChange
        end
        object mmoBNPubProduct: TMemo
          Left = 128
          Top = 184
          Width = 569
          Height = 45
          ParentShowHint = False
          ScrollBars = ssVertical
          ShowHint = True
          TabOrder = 6
          OnChange = mmoBNChange
        end
        object cbbBits: TComboBox
          Left = 440
          Top = 280
          Width = 57
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 13
          Items.Strings = (
            '4096'
            '2048'
            '1024'
            '512'
            '256'
            '128'
            '64')
        end
        object btnBNLoadKeys: TButton
          Left = 512
          Top = 280
          Width = 67
          Height = 21
          Hint = 'Load Private and Public Keys from PKCS#1 or PKCS#8 PEM File'
          Caption = 'Load PEM'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 14
          OnClick = btnBNLoadKeysClick
        end
        object btnBNSaveKeys: TButton
          Left = 584
          Top = 280
          Width = 67
          Height = 21
          Hint = 'Save Private and Public Keys to PEM File'
          Caption = 'Save PEM'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 15
          OnClick = btnBNSaveKeysClick
        end
        object btnBNLoadPub: TButton
          Left = 704
          Top = 184
          Width = 57
          Height = 21
          Hint = 'Load Public Keys from  PKCS#1 or PKCS#8 Public PEM File'
          Caption = 'Load PEM'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 7
          OnClick = btnBNLoadPubClick
        end
        object btnSavePub: TButton
          Left = 704
          Top = 244
          Width = 57
          Height = 21
          Hint = 'Save Public Keys to Public PEM File'
          Caption = 'Save PEM'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 9
          OnClick = btnSavePubClick
        end
        object cbbSaveFormat: TComboBox
          Left = 696
          Top = 280
          Width = 65
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 16
          Items.Strings = (
            'PKCS#1'
            'PKCS#8')
        end
        object btnBNSendR: TButton
          Left = 696
          Top = 20
          Width = 65
          Height = 21
          Caption = '(p-1)*(q-1)'
          TabOrder = 1
          OnClick = btnBNSendRClick
        end
        object btnPQ: TButton
          Left = 696
          Top = 50
          Width = 65
          Height = 21
          Caption = 'Send p, q'
          TabOrder = 2
          OnClick = btnPQClick
        end
        object cbbMBits: TComboBox
          Left = 200
          Top = 280
          Width = 57
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 11
          Items.Strings = (
            '4096'
            '2048'
            '1024'
            '512'
            '256'
            '128')
        end
        object btnGenByM: TButton
          Left = 16
          Top = 280
          Width = 105
          Height = 21
          Caption = 'Generate RSA Keys'
          TabOrder = 10
          OnClick = btnGenByMClick
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
          Height = 21
          Caption = 'RSA Encrypt'
          TabOrder = 1
          OnClick = btnBNRSAEnClick
        end
        object btnBNRSADe: TButton
          Left = 664
          Top = 66
          Width = 89
          Height = 21
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
        Caption = 'Extended Euclidean Gcd'
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
          Height = 21
          Caption = 'Int64 Extended Euclidean Gcd'
          TabOrder = 2
          OnClick = btnInt64EucClick
        end
        object btnBNGcd: TButton
          Left = 320
          Top = 88
          Width = 193
          Height = 21
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
    object tsModInverse: TTabSheet
      Caption = 'Modular Multiplicative Inverse'
      ImageIndex = 3
      object grpModInverse: TGroupBox
        Left = 16
        Top = 16
        Width = 777
        Height = 401
        Caption = 'Modular Multiplicative Inverse'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        object lbl1: TLabel
          Left = 32
          Top = 40
          Width = 642
          Height = 12
          Caption = 
            '(A   * A)  mod B = 1       A   是 A 针对 B 的模逆元，设为 X。模' +
            '逆元也即不定方程 A * X + B *( -Y) = 1 的解。'
          Font.Charset = GB2312_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = '宋体'
          Font.Style = []
          ParentFont = False
        end
        object lblInverse: TLabel
          Left = 48
          Top = 32
          Width = 9
          Height = 13
          Caption = '-1'
        end
        object lbl2: TLabel
          Left = 200
          Top = 32
          Width = 9
          Height = 13
          Caption = '-1'
        end
        object lblMA: TLabel
          Left = 32
          Top = 72
          Width = 10
          Height = 13
          Caption = 'A:'
        end
        object lblMB: TLabel
          Left = 280
          Top = 72
          Width = 10
          Height = 13
          Caption = 'B:'
        end
        object lblMX: TLabel
          Left = 32
          Top = 160
          Width = 10
          Height = 13
          Caption = 'X:'
        end
        object lblMY: TLabel
          Left = 280
          Top = 160
          Width = 13
          Height = 13
          Caption = '-Y:'
        end
        object lblMX0: TLabel
          Left = 56
          Top = 184
          Width = 10
          Height = 13
          Caption = 'X:'
        end
        object lblMX2: TLabel
          Left = 32
          Top = 216
          Width = 12
          Height = 13
          Caption = 'X'#39':'
        end
        object lblPY: TLabel
          Left = 280
          Top = 216
          Width = 10
          Height = 13
          Caption = 'Y:'
        end
        object edtMa: TEdit
          Left = 56
          Top = 68
          Width = 193
          Height = 21
          TabOrder = 0
          Text = '7'
        end
        object edtMb: TEdit
          Left = 320
          Top = 68
          Width = 193
          Height = 21
          TabOrder = 1
          Text = '26'
        end
        object btnMInt64MI: TButton
          Left = 56
          Top = 112
          Width = 449
          Height = 21
          Caption = 'Int64 Extended Euclidean Gcd to Calc  X'
          TabOrder = 2
          OnClick = btnMInt64MIClick
        end
        object edtMX: TEdit
          Left = 56
          Top = 156
          Width = 193
          Height = 21
          ReadOnly = True
          TabOrder = 3
        end
        object edtMY: TEdit
          Left = 320
          Top = 156
          Width = 193
          Height = 21
          ReadOnly = True
          TabOrder = 4
        end
        object edtMXP: TEdit
          Left = 56
          Top = 212
          Width = 193
          Height = 21
          ReadOnly = True
          TabOrder = 5
        end
        object edtPY: TEdit
          Left = 320
          Top = 212
          Width = 193
          Height = 21
          ReadOnly = True
          TabOrder = 6
        end
      end
    end
  end
  object dlgOpenPEM: TOpenDialog
    Left = 492
    Top = 240
  end
  object dlgSavePEM: TSaveDialog
    Left = 532
    Top = 240
  end
end
