object FormLattice: TFormLattice
  Left = 192
  Top = 111
  Width = 834
  Height = 615
  Caption = 'Lattice'
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
  object pgcLattice: TPageControl
    Left = 8
    Top = 8
    Width = 809
    Height = 569
    ActivePage = tsNTRU
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object tsNTRU: TTabSheet
      Caption = 'NTRU'
      object grpNTRU: TGroupBox
        Left = 8
        Top = 4
        Width = 785
        Height = 529
        Anchors = [akLeft, akTop, akRight, akBottom]
        Caption = 'NTRU Encryption/Decryption'
        TabOrder = 0
        object lblNTRUType: TLabel
          Left = 16
          Top = 24
          Width = 61
          Height = 13
          Caption = 'NTRU Type:'
        end
        object lblPrivateKey: TLabel
          Left = 16
          Top = 56
          Width = 57
          Height = 13
          Caption = 'Private Key:'
        end
        object lblPublicKey: TLabel
          Left = 16
          Top = 136
          Width = 53
          Height = 13
          Caption = 'Public Key:'
        end
        object lblNTRUMessage: TLabel
          Left = 16
          Top = 216
          Width = 46
          Height = 13
          Caption = 'Message:'
        end
        object lblNTRUPolynomial: TLabel
          Left = 16
          Top = 248
          Width = 53
          Height = 13
          Caption = 'Polynomial:'
        end
        object lblNTRUEnc: TLabel
          Left = 16
          Top = 328
          Width = 51
          Height = 13
          Caption = 'Encrypted:'
        end
        object lblNTRUDec: TLabel
          Left = 16
          Top = 408
          Width = 52
          Height = 13
          Caption = 'Decrypted:'
        end
        object lblNTRUMessageDec: TLabel
          Left = 16
          Top = 488
          Width = 46
          Height = 13
          Caption = 'Message:'
        end
        object cbbNTRUType: TComboBox
          Left = 96
          Top = 24
          Width = 145
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 0
          OnChange = cbbNTRUTypeChange
        end
        object mmoNTRUPrivateKeyF: TMemo
          Left = 96
          Top = 56
          Width = 329
          Height = 73
          ScrollBars = ssVertical
          TabOrder = 1
        end
        object mmoNTRUPublicKey: TMemo
          Left = 96
          Top = 136
          Width = 665
          Height = 73
          ScrollBars = ssVertical
          TabOrder = 2
        end
        object edtNTRUMessage: TEdit
          Left = 96
          Top = 216
          Width = 121
          Height = 21
          TabOrder = 3
          Text = 'CnPack'
        end
        object mmoNTRUPolynomial: TMemo
          Left = 96
          Top = 248
          Width = 665
          Height = 73
          ScrollBars = ssVertical
          TabOrder = 4
        end
        object mmoNTRUEnc: TMemo
          Left = 96
          Top = 328
          Width = 665
          Height = 73
          ScrollBars = ssVertical
          TabOrder = 5
        end
        object btnNTRUEncrypt: TButton
          Left = 232
          Top = 216
          Width = 193
          Height = 25
          Caption = 'NTRU Encrypt/Decrypt Polynomial'
          TabOrder = 6
          OnClick = btnNTRUEncryptClick
        end
        object mmoNTRUDec: TMemo
          Left = 96
          Top = 408
          Width = 665
          Height = 73
          ScrollBars = ssVertical
          TabOrder = 7
        end
        object edtNTRUMessageDec: TEdit
          Left = 96
          Top = 488
          Width = 121
          Height = 21
          TabOrder = 8
        end
        object btnNTRUGenerateKeys: TButton
          Left = 256
          Top = 24
          Width = 97
          Height = 25
          Caption = 'Generate Keys'
          TabOrder = 9
          OnClick = btnNTRUGenerateKeysClick
        end
        object mmoNTRUPrivateKeyG: TMemo
          Left = 432
          Top = 56
          Width = 329
          Height = 73
          ScrollBars = ssVertical
          TabOrder = 10
        end
        object btnNTRUEncryptBytes: TButton
          Left = 592
          Top = 216
          Width = 169
          Height = 25
          Caption = 'NTRU Encrypt/Decrypt Bytes'
          TabOrder = 11
          OnClick = btnNTRUEncryptBytesClick
        end
      end
    end
    object tsBasic: TTabSheet
      Caption = 'Basic'
      ImageIndex = 1
      object btnInt64GaussianReduceBasis: TButton
        Left = 24
        Top = 16
        Width = 177
        Height = 25
        Caption = 'Int64 Gaussian Reduce Basis'
        TabOrder = 0
        OnClick = btnInt64GaussianReduceBasisClick
      end
      object btnGenNTRUKeys: TButton
        Left = 24
        Top = 56
        Width = 177
        Height = 25
        Caption = 'Generate NTRU Keys'
        TabOrder = 1
        OnClick = btnGenNTRUKeysClick
      end
      object btnPolynomialNTRU: TButton
        Left = 24
        Top = 96
        Width = 177
        Height = 25
        Caption = 'Polynomial NTRU Enc/Dec'
        TabOrder = 2
        OnClick = btnPolynomialNTRUClick
      end
      object btnSimpleTest2: TButton
        Left = 304
        Top = 56
        Width = 177
        Height = 25
        Caption = 'Simple Polynomial NTRU Attack'
        TabOrder = 3
        OnClick = btnSimpleTest2Click
      end
      object btnSimpleTest: TButton
        Left = 304
        Top = 16
        Width = 177
        Height = 25
        Caption = 'Simple NTRU Attack'
        TabOrder = 4
        OnClick = btnSimpleTestClick
      end
    end
    object tsMLKEM: TTabSheet
      Caption = 'MLKEM'
      ImageIndex = 2
      object grpMLKEM: TGroupBox
        Left = 8
        Top = 4
        Width = 785
        Height = 529
        Caption = 'Module-Lattice-based Key Encapsulation Mechanism'
        TabOrder = 0
        object btnCompressTest: TButton
          Left = 16
          Top = 32
          Width = 81
          Height = 25
          Caption = 'Compress Test'
          TabOrder = 0
          OnClick = btnCompressTestClick
        end
        object mmoMLKEM: TMemo
          Left = 16
          Top = 136
          Width = 201
          Height = 377
          ScrollBars = ssVertical
          TabOrder = 1
        end
        object btnDeCompressTest: TButton
          Left = 128
          Top = 32
          Width = 89
          Height = 25
          Caption = 'Decompress Test'
          TabOrder = 2
          OnClick = btnDeCompressTestClick
        end
        object btnMLKEMSamplePolyCBD: TButton
          Left = 16
          Top = 72
          Width = 113
          Height = 25
          Caption = 'Sample PolyCBD'
          TabOrder = 3
          OnClick = btnMLKEMSamplePolyCBDClick
        end
        object edtSamleEta: TEdit
          Left = 144
          Top = 74
          Width = 73
          Height = 21
          TabOrder = 4
          Text = '2'
        end
        object edtMLKEMD: TEdit
          Left = 100
          Top = 34
          Width = 25
          Height = 21
          TabOrder = 5
          Text = '11'
        end
        object btnMLKEMSampleNtt: TButton
          Left = 16
          Top = 104
          Width = 113
          Height = 25
          Caption = 'Sample NTT'
          TabOrder = 6
          OnClick = btnMLKEMSampleNttClick
        end
        object btnMLKEMKeyGen: TButton
          Left = 240
          Top = 32
          Width = 161
          Height = 25
          Caption = 'MLKEM KeyGen'
          TabOrder = 7
          OnClick = btnMLKEMKeyGenClick
        end
        object mmoMLKEMKeys: TMemo
          Left = 240
          Top = 64
          Width = 529
          Height = 145
          TabOrder = 8
        end
      end
    end
  end
end
