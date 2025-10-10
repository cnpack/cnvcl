object FormRSA: TFormRSA
  Left = 234
  Top = 61
  BorderStyle = bsDialog
  Caption = 'RSA Demo'
  ClientHeight = 627
  ClientWidth = 1041
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
    Width = 1007
    Height = 589
    ActivePage = tsInt64RSA
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object tsInt64RSA: TTabSheet
      Caption = 'Int64 RSA'
      object grpKeys: TGroupBox
        Left = 16
        Top = 16
        Width = 857
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
          ShowHint = True
          TabOrder = 0
          OnChange = edtBNChange
        end
        object edtPrime2: TEdit
          Left = 480
          Top = 20
          Width = 361
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          ParentShowHint = False
          ShowHint = True
          TabOrder = 1
          OnChange = edtBNChange
        end
        object edtPrivProduct: TEdit
          Left = 128
          Top = 52
          Width = 713
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          ParentShowHint = False
          ShowHint = True
          TabOrder = 2
          OnChange = edtBNChange
        end
        object edtPrivExp: TEdit
          Left = 128
          Top = 84
          Width = 713
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          ParentShowHint = False
          ShowHint = True
          TabOrder = 3
          OnChange = edtBNChange
        end
        object edtPubProduct: TEdit
          Left = 128
          Top = 124
          Width = 713
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          ParentShowHint = False
          ShowHint = True
          TabOrder = 4
          OnChange = edtBNChange
        end
        object edtPubExp: TEdit
          Left = 128
          Top = 156
          Width = 713
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          ParentShowHint = False
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
          Left = 568
          Top = 192
          Width = 177
          Height = 21
          Caption = 'Send (p-1)*(q-1) to Euclidean B'
          TabOrder = 8
          OnClick = btnSendRClick
        end
        object chkN64: TCheckBox
          Left = 432
          Top = 192
          Width = 97
          Height = 17
          Caption = '* 64 Bits'
          TabOrder = 7
        end
        object btnInt64VerifyKeys: TButton
          Left = 760
          Top = 192
          Width = 81
          Height = 21
          Hint = 'Verify Loaded RSA Private and Public Keys'
          Anchors = [akTop, akRight]
          Caption = 'Verify Int64 Keys'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 9
          OnClick = btnInt64VerifyKeysClick
        end
      end
      object grpCrypt: TGroupBox
        Left = 16
        Top = 420
        Width = 967
        Height = 121
        Anchors = [akLeft, akRight, akBottom]
        Caption = 'Crypt'
        TabOrder = 2
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
          Width = 529
          Height = 21
          TabOrder = 0
          Text = '12345678987654321'
        end
        object edtRes: TEdit
          Left = 64
          Top = 52
          Width = 529
          Height = 21
          TabOrder = 3
        end
        object edtDataBack: TEdit
          Left = 64
          Top = 84
          Width = 529
          Height = 21
          ReadOnly = True
          TabOrder = 5
        end
        object btnRSAEn: TButton
          Left = 616
          Top = 34
          Width = 89
          Height = 21
          Caption = 'RSA Encrypt'
          TabOrder = 1
          OnClick = btnRSAEnClick
        end
        object btnRSADe: TButton
          Left = 616
          Top = 66
          Width = 89
          Height = 21
          Caption = 'RSA Decrypt'
          TabOrder = 4
          OnClick = btnRSADeClick
        end
        object chkPureUInt64: TCheckBox
          Left = 712
          Top = 48
          Width = 57
          Height = 17
          Caption = 'UInt64'
          Enabled = False
          TabOrder = 2
        end
      end
      object btnInt64Sample: TButton
        Left = 896
        Top = 24
        Width = 75
        Height = 25
        Caption = 'Int64 Sample'
        TabOrder = 1
        OnClick = btnInt64SampleClick
      end
    end
    object tsRSA: TTabSheet
      Caption = 'Big Number RSA'
      ImageIndex = 1
      object grpBNKeys: TGroupBox
        Left = 16
        Top = 16
        Width = 969
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
          Left = 768
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
        object lblSaveCrypt: TLabel
          Left = 878
          Top = 284
          Width = 4
          Height = 13
          Caption = '*'
        end
        object bvlLoadPEM: TBevel
          Left = 688
          Top = 280
          Width = 9
          Height = 25
          Shape = bsLeftLine
        end
        object lblKeyHash: TLabel
          Left = 584
          Top = 284
          Width = 46
          Height = 13
          Caption = 'KeyHash:'
        end
        object edtBNPrime1: TEdit
          Left = 128
          Top = 20
          Width = 745
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          ParentShowHint = False
          ReadOnly = True
          ShowHint = True
          TabOrder = 0
          OnChange = edtBNChange
        end
        object edtBNPrime2: TEdit
          Left = 128
          Top = 52
          Width = 745
          Height = 21
          Anchors = [akLeft, akTop, akRight]
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
          TabOrder = 13
          OnClick = btnBNGenClick
        end
        object edtBNPrivExp: TEdit
          Left = 128
          Top = 148
          Width = 825
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          ParentShowHint = False
          ReadOnly = True
          ShowHint = True
          TabOrder = 5
          OnChange = edtBNChange
        end
        object edtBNPubExp: TEdit
          Left = 128
          Top = 244
          Width = 761
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          ParentShowHint = False
          ReadOnly = True
          ShowHint = True
          TabOrder = 9
          OnChange = edtBNChange
        end
        object mmoBNPrivProduct: TMemo
          Left = 128
          Top = 88
          Width = 825
          Height = 45
          Anchors = [akLeft, akTop, akRight]
          ParentShowHint = False
          ScrollBars = ssVertical
          ShowHint = True
          TabOrder = 4
          OnChange = mmoBNChange
        end
        object mmoBNPubProduct: TMemo
          Left = 128
          Top = 184
          Width = 761
          Height = 45
          Anchors = [akLeft, akTop, akRight]
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
          TabOrder = 14
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
          TabOrder = 15
          OnClick = btnBNLoadKeysClick
        end
        object btnBNSaveKeys: TButton
          Left = 696
          Top = 280
          Width = 67
          Height = 21
          Hint = 'Save Private and Public Keys to PEM File'
          Caption = 'Save PEM'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 17
          OnClick = btnBNSaveKeysClick
        end
        object btnBNLoadPub: TButton
          Left = 896
          Top = 184
          Width = 57
          Height = 21
          Hint = 'Load Public Keys from  PKCS#1 or PKCS#8 Public PEM File'
          Anchors = [akTop, akRight]
          Caption = 'Load PEM'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 7
          OnClick = btnBNLoadPubClick
        end
        object btnSavePub: TButton
          Left = 896
          Top = 244
          Width = 57
          Height = 21
          Hint = 'Save Public Keys to Public PEM File'
          Anchors = [akTop, akRight]
          Caption = 'Save PEM'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 10
          OnClick = btnSavePubClick
        end
        object cbbSaveFormat: TComboBox
          Left = 808
          Top = 280
          Width = 65
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 18
          Items.Strings = (
            'PKCS#1'
            'PKCS#8')
        end
        object btnBNSendR: TButton
          Left = 888
          Top = 20
          Width = 65
          Height = 21
          Anchors = [akTop, akRight]
          Caption = '(p-1)*(q-1)'
          TabOrder = 1
          OnClick = btnBNSendRClick
        end
        object btnPQ: TButton
          Left = 888
          Top = 50
          Width = 65
          Height = 21
          Anchors = [akTop, akRight]
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
          TabOrder = 12
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
          TabOrder = 11
          OnClick = btnGenByMClick
        end
        object cbbSaveCrypt: TComboBox
          Left = 888
          Top = 280
          Width = 65
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 19
          Items.Strings = (
            'NO Crypt'
            'DES'
            '3DES'
            'AES128'
            'AES192'
            'AES256')
        end
        object cbbLoadKeyHash: TComboBox
          Left = 632
          Top = 280
          Width = 49
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 16
          Items.Strings = (
            'MD5'
            'SHA256')
        end
        object btnBNVerifyKeys: TButton
          Left = 896
          Top = 208
          Width = 57
          Height = 21
          Hint = 'Verify Loaded RSA Private and Public Keys'
          Anchors = [akTop, akRight]
          Caption = 'Verify Keys'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 8
          OnClick = btnBNVerifyKeysClick
        end
      end
      object pgc2: TPageControl
        Left = 16
        Top = 344
        Width = 967
        Height = 205
        ActivePage = tsData
        Anchors = [akLeft, akTop, akRight, akBottom]
        Images = ilCrypt
        MultiLine = True
        TabOrder = 1
        TabPosition = tpRight
        object tsData: TTabSheet
          Hint = 'Big Integer Encryption/Decryption'
          ParentShowHint = False
          ShowHint = True
          object lblBNResult: TLabel
            Left = 10
            Top = 62
            Width = 33
            Height = 13
            Caption = 'Result:'
          end
          object lblBNInteger: TLabel
            Left = 10
            Top = 24
            Width = 41
            Height = 13
            Caption = 'Decimal:'
          end
          object lblBNDecrypt: TLabel
            Left = 10
            Top = 100
            Width = 41
            Height = 13
            Caption = 'Decimal:'
          end
          object btnBNRSAEn: TButton
            Left = 648
            Top = 40
            Width = 81
            Height = 21
            Caption = 'RSA Encrypt'
            TabOrder = 1
            OnClick = btnBNRSAEnClick
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
            Top = 58
            Width = 577
            Height = 21
            TabOrder = 2
          end
          object edtBNDataBack: TEdit
            Left = 64
            Top = 96
            Width = 577
            Height = 21
            ReadOnly = True
            TabOrder = 4
          end
          object btnBNRSADe: TButton
            Left = 648
            Top = 72
            Width = 81
            Height = 21
            Caption = 'RSA Decrypt'
            TabOrder = 3
            OnClick = btnBNRSADeClick
          end
        end
        object tsFile: TTabSheet
          Hint = 'File Data Encryption/Decryption'
          ImageIndex = 1
          ParentShowHint = False
          ShowHint = True
          object lblFile1: TLabel
            Left = 10
            Top = 12
            Width = 57
            Height = 13
            Caption = 'Original File:'
          end
          object lblFile2: TLabel
            Left = 10
            Top = 76
            Width = 70
            Height = 13
            Caption = 'Encrypted File:'
          end
          object edtFile1: TEdit
            Left = 88
            Top = 8
            Width = 553
            Height = 21
            TabOrder = 0
          end
          object edtFile2: TEdit
            Left = 88
            Top = 72
            Width = 553
            Height = 21
            ReadOnly = True
            TabOrder = 5
          end
          object btnBrowse1: TButton
            Left = 656
            Top = 8
            Width = 75
            Height = 21
            Caption = 'Browse'
            TabOrder = 1
            OnClick = btnBrowse1Click
          end
          object btnBrowse2: TButton
            Left = 656
            Top = 72
            Width = 75
            Height = 21
            Caption = 'Browse'
            TabOrder = 6
            OnClick = btnBrowse2Click
          end
          object btnPrivCrypt: TBitBtn
            Left = 88
            Top = 40
            Width = 137
            Height = 21
            Caption = 'Crypt Using Private Key'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clGray
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
            TabOrder = 2
            OnClick = btnPrivCryptClick
          end
          object btnPubCrypt: TButton
            Left = 248
            Top = 40
            Width = 137
            Height = 21
            Caption = 'Crypt Using Public Key'
            TabOrder = 3
            OnClick = btnPubCryptClick
          end
          object btnDePrivate: TButton
            Left = 88
            Top = 104
            Width = 137
            Height = 21
            Caption = 'Decrypt Using Private Key'
            TabOrder = 7
            OnClick = btnDePrivateClick
          end
          object btnDePub: TBitBtn
            Left = 248
            Top = 104
            Width = 137
            Height = 21
            Caption = 'Decrypt Using Public Key'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clGray
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
            TabOrder = 8
            OnClick = btnDePubClick
          end
          object chkOAEP: TCheckBox
            Left = 392
            Top = 42
            Width = 97
            Height = 17
            Caption = 'OAEP Padding'
            TabOrder = 4
          end
          object btnPubCryptLong: TButton
            Left = 512
            Top = 40
            Width = 257
            Height = 21
            Caption = 'Crypt Big File with PKCS1 Block Using Public Key'
            TabOrder = 9
            OnClick = btnPubCryptLongClick
          end
          object btnPrivDecryptLong: TButton
            Left = 512
            Top = 104
            Width = 257
            Height = 21
            Caption = 'Decrypt Big File with PKCS1 Block Using Private Key'
            TabOrder = 10
            OnClick = btnPrivDecryptLongClick
          end
        end
        object tsSign: TTabSheet
          Hint = 'Sign/Verify File'
          ImageIndex = 2
          ParentShowHint = False
          ShowHint = True
          object lblSignFile: TLabel
            Left = 10
            Top = 12
            Width = 57
            Height = 13
            Caption = 'Original File:'
          end
          object lblSignature: TLabel
            Left = 10
            Top = 76
            Width = 67
            Height = 13
            Caption = 'Signature File:'
          end
          object lblSigMethod: TLabel
            Left = 662
            Top = 42
            Width = 94
            Height = 13
            Anchors = [akTop, akRight]
            Caption = 'Signature Algorithm:'
          end
          object edtSignFile: TEdit
            Left = 88
            Top = 8
            Width = 735
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 0
          end
          object btnSignBrowse: TButton
            Left = 830
            Top = 8
            Width = 75
            Height = 21
            Anchors = [akTop, akRight]
            Caption = 'Browse'
            TabOrder = 1
            OnClick = btnSignBrowseClick
          end
          object btnSignPrivate: TButton
            Left = 88
            Top = 40
            Width = 137
            Height = 21
            Caption = 'Sign Using Private Key'
            TabOrder = 2
            OnClick = btnSignPrivateClick
          end
          object edtSigFile: TEdit
            Left = 88
            Top = 72
            Width = 735
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            ReadOnly = True
            TabOrder = 4
          end
          object btnSignatureBrowse: TButton
            Left = 830
            Top = 72
            Width = 75
            Height = 21
            Anchors = [akTop, akRight]
            Caption = 'Browse'
            TabOrder = 5
            OnClick = btnSignatureBrowseClick
          end
          object btnPrivVerify: TButton
            Left = 256
            Top = 104
            Width = 137
            Height = 21
            Caption = 'Verify Using Private Key'
            Enabled = False
            TabOrder = 7
            OnClick = btnPrivVerifyClick
          end
          object btnPubVerify: TButton
            Left = 88
            Top = 104
            Width = 137
            Height = 21
            Caption = 'Verify Using Public Key'
            TabOrder = 6
            OnClick = btnPubVerifyClick
          end
          object cbbSig: TComboBox
            Left = 766
            Top = 40
            Width = 65
            Height = 21
            Style = csDropDownList
            Anchors = [akTop, akRight]
            ItemHeight = 13
            TabOrder = 3
            Items.Strings = (
              '<None>'
              'MD5'
              'SHA1'
              'SHA256')
          end
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
          Left = 304
          Top = 112
          Width = 209
          Height = 21
          Caption = 'Int64 Extended Euclidean Gcd to Calc  X'
          TabOrder = 5
          OnClick = btnMInt64MIClick
        end
        object edtMX: TEdit
          Left = 56
          Top = 156
          Width = 193
          Height = 21
          ReadOnly = True
          TabOrder = 6
        end
        object edtMY: TEdit
          Left = 320
          Top = 156
          Width = 193
          Height = 21
          ReadOnly = True
          TabOrder = 7
        end
        object edtMXP: TEdit
          Left = 56
          Top = 212
          Width = 193
          Height = 21
          ReadOnly = True
          TabOrder = 8
        end
        object edtPY: TEdit
          Left = 320
          Top = 212
          Width = 193
          Height = 21
          ReadOnly = True
          TabOrder = 9
        end
        object btnMInt32MI: TButton
          Left = 56
          Top = 112
          Width = 209
          Height = 21
          Caption = 'UInt32 Extended Euclidean Gcd to Calc  X'
          TabOrder = 4
          OnClick = btnMInt32MIClick
        end
        object btnUInt32MI: TButton
          Left = 536
          Top = 68
          Width = 75
          Height = 21
          Caption = 'UInt32 MI'
          TabOrder = 2
          OnClick = btnUInt32MIClick
        end
        object btnInt64MI: TButton
          Left = 632
          Top = 68
          Width = 75
          Height = 21
          Caption = 'Int64 MI'
          TabOrder = 3
          OnClick = btnInt64MIClick
        end
      end
    end
    object tsDiffieHellman: TTabSheet
      Caption = 'Diffie Hellman / Chameleon Hash'
      ImageIndex = 4
      object lblSqrt: TLabel
        Left = 16
        Top = 248
        Width = 45
        Height = 13
        Caption = 'Fast Sqrt:'
      end
      object grpFactors: TGroupBox
        Left = 16
        Top = 16
        Width = 761
        Height = 209
        Caption = 'Find Factors'
        TabOrder = 0
        object lblFactorNumber: TLabel
          Left = 16
          Top = 24
          Width = 40
          Height = 13
          Caption = 'Number:'
        end
        object lblDHPrime: TLabel
          Left = 16
          Top = 60
          Width = 29
          Height = 13
          Caption = 'Prime:'
        end
        object lblDHRoot: TLabel
          Left = 440
          Top = 60
          Width = 46
          Height = 13
          Caption = 'Min Root:'
        end
        object lblDHA: TLabel
          Left = 16
          Top = 96
          Width = 10
          Height = 13
          Caption = 'A:'
        end
        object lblXA: TLabel
          Left = 48
          Top = 96
          Width = 13
          Height = 13
          Caption = 'Xa'
        end
        object lblDHB: TLabel
          Left = 16
          Top = 136
          Width = 10
          Height = 13
          Caption = 'B:'
        end
        object lblXb: TLabel
          Left = 48
          Top = 136
          Width = 13
          Height = 13
          Caption = 'Xb'
        end
        object lblDHBits: TLabel
          Left = 16
          Top = 176
          Width = 20
          Height = 13
          Caption = 'Bits:'
        end
        object edtDHNumber: TEdit
          Left = 72
          Top = 20
          Width = 545
          Height = 21
          TabOrder = 0
          Text = '291851311113628095805283489717894794296'
        end
        object btnFindFactors: TButton
          Left = 632
          Top = 20
          Width = 113
          Height = 21
          Caption = 'Find Factors'
          TabOrder = 1
          OnClick = btnFindFactorsClick
        end
        object edtDHPrime: TEdit
          Left = 72
          Top = 56
          Width = 353
          Height = 21
          TabOrder = 2
        end
        object edtDHRoot: TEdit
          Left = 496
          Top = 56
          Width = 121
          Height = 21
          TabOrder = 3
        end
        object btnGenInt64DH: TButton
          Left = 632
          Top = 56
          Width = 113
          Height = 21
          Caption = 'Generate DH'
          TabOrder = 4
          OnClick = btnGenInt64DHClick
        end
        object edtDHXa: TEdit
          Left = 72
          Top = 92
          Width = 163
          Height = 21
          TabOrder = 5
          Text = '123456'
        end
        object edtDHXb: TEdit
          Left = 72
          Top = 132
          Width = 163
          Height = 21
          TabOrder = 11
          Text = '654321'
        end
        object btnCalcYb: TButton
          Left = 250
          Top = 132
          Width = 49
          Height = 21
          Caption = 'Calc Yb'
          TabOrder = 12
          OnClick = btnCalcYbClick
        end
        object btnCalcXA: TButton
          Left = 250
          Top = 92
          Width = 49
          Height = 21
          Caption = 'Calc Ya'
          TabOrder = 6
          OnClick = btnCalcXAClick
        end
        object edtDHYa: TEdit
          Left = 314
          Top = 92
          Width = 175
          Height = 21
          TabOrder = 7
        end
        object edtDHYb: TEdit
          Left = 314
          Top = 132
          Width = 175
          Height = 21
          TabOrder = 13
        end
        object btnDHBCK: TButton
          Left = 502
          Top = 132
          Width = 65
          Height = 21
          Caption = 'B Calc Key'
          TabOrder = 14
          OnClick = btnDHBCKClick
        end
        object btnDHACKey: TButton
          Left = 502
          Top = 92
          Width = 65
          Height = 21
          Caption = 'A Calc Key'
          TabOrder = 8
          OnClick = btnDHACKeyClick
        end
        object edtAKey: TEdit
          Left = 581
          Top = 92
          Width = 163
          Height = 21
          TabOrder = 9
        end
        object edtBKey: TEdit
          Left = 581
          Top = 132
          Width = 163
          Height = 21
          TabOrder = 15
        end
        object cbbDHBits: TComboBox
          Left = 72
          Top = 172
          Width = 145
          Height = 21
          ItemHeight = 13
          TabOrder = 16
          Text = '64'
          Items.Strings = (
            '32'
            '64'
            '128'
            '256')
        end
        object btnDHRand: TButton
          Left = 40
          Top = 112
          Width = 25
          Height = 21
          Caption = 'R'
          TabOrder = 10
          OnClick = btnDHRandClick
        end
      end
      object edtFastSqrt: TEdit
        Left = 80
        Top = 244
        Width = 145
        Height = 21
        TabOrder = 1
        Text = '10000'
      end
      object btnFastSqrt: TButton
        Left = 256
        Top = 244
        Width = 75
        Height = 21
        Caption = 'Fast Sqrt'
        TabOrder = 2
        OnClick = btnFastSqrtClick
      end
      object grpChameleonHash: TGroupBox
        Left = 16
        Top = 280
        Width = 761
        Height = 265
        Caption = 'Chameleon Hash'
        TabOrder = 3
        object lblCHPrime: TLabel
          Left = 16
          Top = 60
          Width = 29
          Height = 13
          Caption = 'Prime:'
        end
        object lblCHRoot: TLabel
          Left = 440
          Top = 60
          Width = 46
          Height = 13
          Caption = 'Min Root:'
        end
        object lblCHNumber: TLabel
          Left = 16
          Top = 24
          Width = 25
          Height = 13
          Caption = 'Num:'
        end
        object lblCHSecKey: TLabel
          Left = 16
          Top = 96
          Width = 55
          Height = 13
          Caption = 'Secret Key:'
        end
        object lblCHRandom1: TLabel
          Left = 16
          Top = 128
          Width = 49
          Height = 13
          Caption = 'Random1:'
        end
        object lblCHHash: TLabel
          Left = 16
          Top = 160
          Width = 28
          Height = 13
          Caption = 'Hash:'
        end
        object lblCHRandom2: TLabel
          Left = 16
          Top = 232
          Width = 49
          Height = 13
          Caption = 'Random2:'
        end
        object bvl2: TBevel
          Left = 16
          Top = 184
          Width = 729
          Height = 9
          Shape = bsTopLine
        end
        object lblCHNewNumber: TLabel
          Left = 16
          Top = 200
          Width = 50
          Height = 13
          Caption = 'New Num:'
        end
        object edtCHPrime: TEdit
          Left = 72
          Top = 56
          Width = 353
          Height = 21
          TabOrder = 1
        end
        object edtCHRoot: TEdit
          Left = 496
          Top = 56
          Width = 121
          Height = 21
          TabOrder = 2
        end
        object btnGenCH: TButton
          Left = 632
          Top = 56
          Width = 113
          Height = 21
          Caption = 'Generate CH'
          TabOrder = 3
          OnClick = btnGenCHClick
        end
        object edtCHNumber: TEdit
          Left = 72
          Top = 20
          Width = 545
          Height = 21
          TabOrder = 0
          Text = '73618513111136280958052434894378447995623'
        end
        object edtCHSecKey: TEdit
          Left = 72
          Top = 92
          Width = 545
          Height = 21
          TabOrder = 4
          Text = '7934806187680192847293875645476892163274087655'
        end
        object edtCHRandom1: TEdit
          Left = 72
          Top = 124
          Width = 545
          Height = 21
          TabOrder = 5
          Text = '87341342568764456767764521100906219000756'
        end
        object btnCalcCH: TButton
          Left = 632
          Top = 126
          Width = 113
          Height = 21
          Caption = 'Calc Hash'
          TabOrder = 6
          OnClick = btnCalcCHClick
        end
        object edtCHHash: TEdit
          Left = 72
          Top = 156
          Width = 545
          Height = 21
          TabOrder = 7
        end
        object edtCHRandom2: TEdit
          Left = 72
          Top = 228
          Width = 545
          Height = 21
          TabOrder = 10
        end
        object btnFindRandom: TButton
          Left = 632
          Top = 198
          Width = 113
          Height = 21
          Caption = 'Find Random'
          TabOrder = 9
          OnClick = btnFindRandomClick
        end
        object edtCHNewNum: TEdit
          Left = 72
          Top = 196
          Width = 545
          Height = 21
          TabOrder = 8
          Text = '961397411368986421346864224325900985312468'
        end
        object btnCHVerify: TButton
          Left = 632
          Top = 230
          Width = 113
          Height = 21
          Caption = 'Verify Hash'
          TabOrder = 11
          OnClick = btnCHVerifyClick
        end
      end
    end
  end
  object dlgOpenPEM: TOpenDialog
    Left = 540
    Top = 240
  end
  object dlgSavePEM: TSaveDialog
    Left = 564
    Top = 240
  end
  object ilCrypt: TImageList
    Left = 228
    Top = 152
    Bitmap = {
      494C010103000500040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000002000000001002000000000000020
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B5848400B584
      8400B5848400B5848400B5848400B5848400B5848400B5848400B5848400B584
      8400B5848400B5848400B5848400000000007B7373004A4A4A00524A4A00524A
      4A00524A4A0052524A0052524A00524A4A00524A4A00524A4A00524A4A00524A
      4200000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000945A5A00945A5A00945A5A00945A
      5A00945A5A00945A5A00945A5A00945A5A00945A5A00945A5A00000000000000
      0000000000000000000000000000000000000000000000000000C6A59C00FFEF
      D600F7E7C600F7DEBD00F7DEB500F7D6AD00F7D6A500EFCE9C00EFCE9400EFCE
      9400EFCE9400F7D69C00B584840000000000DECECE00FFFFFF00FFFFFF00FFFF
      FF00FFFFF700FFFFF700FFFFF700FFFFEF00FFF7E700FFEFD600FFEFD600524A
      4200000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000945A5A00EFDECE00EFD6BD00E7CE
      B500E7C6AD00E7BDA500DEBD9C00DEB58C00D6AD8400945A5A00000000000000
      0000000000000000000000000000000000000000000000000000C6A59C00FFEF
      D600C6A59C00C6A59C00C6A59C00C6A59C00C6A59C00C6A59C00C6A59C00C6A5
      9C00C6A59C00EFCE9C00B584840000000000D6CEC600FFFFFF00C6ADA500C6AD
      A500FFFFFF00C6ADA500C6ADA500C6ADA500C6ADA500C6ADA500FFDECE00524A
      4200000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000945A5A00FFF7EF00FFEFE700F7E7
      D600F7E7C600F7DEBD00F7D6AD00EFCEA500EFC69400945A5A00000000000000
      0000000000000000000000000000000000000000000000000000C6ADA500FFEF
      E700F7E7D600F7E7CE00F7DEC600F7DEBD00F7D6B500F7D6AD00EFCE9C00EFCE
      9C00EFCE9400EFCE9C00B584840000000000D6CEC600FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFF7EF00FFEFE700FFEFDE00FFE7D600FFE7D600524A
      4200000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000945A5A00945A5A00945A5A00945A
      5A00945A5A00945A5A00945A5A00945A5A00945A5A00945A5A00000000000000
      0000000000000000000000000000000000000000000000000000C6ADA500FFF7
      E700F7E7D600F7E7CE00F7E7C600F7DEC600F7DEB500F7D6B500F7D6AD00EFCE
      9C00EFCE9C00EFCE9400B584840000000000D6CEC600FFFFFF00C6ADA500C6AD
      A500FFFFFF00C6ADA500C6ADA500C6ADA500C6ADA500C6ADA500FFE7D600524A
      4200000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000CEB5AD00FFFF
      F700C6A59C00C6A59C00C6A59C00C6A59C00C6A59C00C6A59C00C6A59C00C6A5
      9C00C6A59C00EFCE9C00B584840000000000D6C6C600FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFF7F700FFF7EF00FFF7EF00FFEFDE00524A
      4200000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000945A5A00945A5A00945A5A00945A
      5A00945A5A00945A5A00945A5A00945A5A00945A5A00945A5A00000000000000
      0000000000000000000000000000000000000000000000000000D6B5AD00FFFF
      FF00FFF7EF00FFEFE700F7E7D600F7E7CE00F7E7C600F7DEC600F7DEBD00F7D6
      AD00F7D6A500F7D6A500B584840000000000D6CEC600FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00B5ADAD00425A7300E7DED600FFEFE700524A
      4200000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000945A5A00EFDECE00EFD6BD00E7CE
      B500E7C6AD00E7BDA500DEBD9C00DEB58C00D6AD8400945A5A00000000000000
      0000000000000000000000000000000000000000000000000000D6BDB500FFFF
      FF00FFF7F700FFF7EF00FFEFDE00F7E7D600F7E7CE00F7E7C600F7DEC600F7DE
      BD00F7D6B500F7D6AD00B584840000000000D6CEC600FFFFFF00E7EFEF005A73
      8400DEE7E700FFFFFF00C6CECE004A63730029ADD600081018005A5A6300A59C
      94000000000000000000A5BDA500186329000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000945A5A00FFF7EF00FFEFE700F7E7
      D600F7E7C600F7DEBD00F7D6AD00EFCEA500EFC69400945A5A00000000000000
      0000000000000000000000000000000000000000000000000000D6BDB500FFFF
      FF00C6A59C00C6A59C00C6A59C00C6A59C00C6A59C00C6A59C00C6A59C00C6A5
      9C00C6A59C00F7DEB500B584840000000000DE9C7B00F7CEB5008CA5AD0084D6
      E7004A637300A5948C004A63730063C6DE00524A5A0018D6FF00102131001000
      0800002129004A5263005A7B5A00188C31000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000945A5A00945A5A00945A5A00945A
      5A00945A5A00945A5A00945A5A00945A5A00945A5A00945A5A00000000000000
      0000000000000000000000000000000000000000000000000000DEBDB500FFFF
      FF00FFFFFF00FFFFFF00FFF7F700FFEFE700FFEFDE00F7E7D600F7E7CE00F7DE
      C600F7DEC600F7D6B500B584840000000000DE9C7B00FFC6A500E7CEBD008CA5
      AD0084E7F7004A6373007BCEE700526363006BEFFF004239520031B5DE00189C
      CE001094C60010638C0042635200299439000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000DEC6B500FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFF7F700FFEFE700FFEFDE00FFEFDE00FFEF
      D600E7DEC600C6BDAD00B584840000000000DE9C7B00DE9C7B00DE9C7B00DEBD
      AD008CA5AD007BE7FF00528494006BEFFF0031394A006BDEF7005AD6F70042C6
      EF0031BDEF0010ADEF0052948C00319C42000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000945A5A00945A5A00945A5A00945A
      5A00945A5A00945A5A00945A5A00945A5A00945A5A00945A5A00000000000000
      0000000000000000000000000000000000000000000000000000E7C6B500FFFF
      FF00C6A59C00C6A59C00C6A59C00C6A59C00C6A59C00FFF7EF00F7E7D600C6A5
      9400B5948C00B58C8400B5848400000000000000000000000000000000000000
      0000E7EFEF00526B84006BEFFF005A9CAD006BEFFF0073E7FF006BDEF70052CE
      F7004AC6EF0021BDFF005AA5A50042AD52000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000945A5A00EFDECE00EFD6BD00E7CE
      B500E7C6AD00E7BDA500DEBD9C00DEB58C00D6AD8400945A5A00000000000000
      0000000000000000000000000000000000000000000000000000E7C6B500FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFF700E7CECE00BD8C
      7300EFB57300EFA54A00C6846B00000000000000000000000000000000000000
      000000000000B5EFFF005A6B7B006BEFFF006BEFFF006BEFFF0073E7FF0063D6
      F70052BDDE0052738C006B9C8C0084CE84000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000945A5A00FFF7EF00FFEFE700F7E7
      D600F7E7C600F7DEBD00F7D6AD00EFCEA500EFC69400945A5A00000000000000
      0000000000000000000000000000000000000000000000000000EFCEBD00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00E7D6CE00C694
      7B00FFC67300CE94730000000000000000000000000000000000000000000000
      00000000000000000000D6DEDE0084A5B5008494A5008494A5007B8C9C007384
      94005A738400BDCECE0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000945A5A00945A5A00945A5A00945A
      5A00945A5A00945A5A00945A5A00945A5A00945A5A00945A5A00000000000000
      0000000000000000000000000000000000000000000000000000E7C6B500FFF7
      F700FFF7EF00FFF7EF00FFF7EF00FFF7EF00FFF7EF00FFF7EF00E7CECE00C694
      7B00CE9C84000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000E7C6B500EFCE
      B500EFCEB500EFCEB500EFCEB500E7C6B500E7C6B500EFCEB500D6BDB500BD84
      7B00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000200000000100010000000000000100000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000FFFFC001000F00000021C001000F0000
      002DC001000F0000002DC001000F00000021C001000F0000FFFFC001000F0000
      0021C001000F0000002DC001000C0000002DC001000000000021C00100000000
      FFFFC001000000000021C001F0000000002DC001F8000000002DC003FC030000
      0021C007FFFF0000FFFFC00FFFFF000000000000000000000000000000000000
      000000000000}
  end
  object dlgOpenFile: TOpenDialog
    Left = 632
    Top = 398
  end
  object dlgSaveFile: TSaveDialog
    Left = 632
    Top = 454
  end
end
