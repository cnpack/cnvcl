object FormSM2: TFormSM2
  Left = 293
  Top = 149
  BorderStyle = bsDialog
  Caption = 'SM2 Test'
  ClientHeight = 561
  ClientWidth = 952
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
    Width = 914
    Height = 449
    ActivePage = tsTest
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 5
    object tsEncDec: TTabSheet
      Caption = 'Encryttion / Decryption'
      object grpSm2Enc: TGroupBox
        Left = 8
        Top = 8
        Width = 889
        Height = 403
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
          Text = '1234567890'
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
        object mmoSM2Result: TMemo
          Left = 16
          Top = 104
          Width = 857
          Height = 113
          TabOrder = 6
        end
        object btnSM2Decrypt: TButton
          Left = 744
          Top = 232
          Width = 129
          Height = 25
          Caption = 'Decrypt using Private Key'
          TabOrder = 8
          OnClick = btnSM2DecryptClick
        end
        object chkPrefixByte: TCheckBox
          Left = 256
          Top = 56
          Width = 97
          Height = 17
          Caption = '$04'
          Checked = True
          State = cbChecked
          TabOrder = 3
        end
        object btnSM2EncryptFile: TButton
          Left = 16
          Top = 232
          Width = 169
          Height = 25
          Caption = 'Encrypt File using Public Key'
          TabOrder = 7
          OnClick = btnSM2EncryptFileClick
        end
        object btnSM2DecryptFile: TButton
          Left = 16
          Top = 272
          Width = 169
          Height = 25
          Caption = 'Decrypt File using Private Key'
          TabOrder = 9
          OnClick = btnSM2DecryptFileClick
        end
        object rbC1C3C2: TRadioButton
          Left = 608
          Top = 56
          Width = 65
          Height = 17
          Caption = 'C1C3C2'
          Checked = True
          TabOrder = 4
          TabStop = True
        end
        object rbC1C2C3: TRadioButton
          Left = 672
          Top = 56
          Width = 65
          Height = 17
          Caption = 'C1C2C3'
          TabOrder = 5
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
        Height = 403
        Caption = 'Sign / Verify'
        TabOrder = 0
        object bvl3: TBevel
          Left = 16
          Top = 80
          Width = 857
          Height = 9
          Shape = bsTopLine
        end
        object bvl4: TBevel
          Left = 152
          Top = 16
          Width = 17
          Height = 50
          Shape = bsLeftLine
        end
        object lblUserId: TLabel
          Left = 16
          Top = 236
          Width = 37
          Height = 13
          Caption = 'User Id:'
        end
        object lblSM2FileSign: TLabel
          Left = 168
          Top = 36
          Width = 55
          Height = 13
          Caption = 'File to Sign:'
        end
        object bvl5: TBevel
          Left = 684
          Top = 224
          Width = 9
          Height = 41
          Shape = bsLeftLine
        end
        object btnSm2SignVerify: TButton
          Left = 16
          Top = 32
          Width = 113
          Height = 25
          Caption = 'Sm2 Example 256'
          TabOrder = 0
          OnClick = btnSm2SignVerifyClick
        end
        object edtSM2UserId: TEdit
          Left = 64
          Top = 232
          Width = 161
          Height = 21
          TabOrder = 4
          Text = 'CnPack Team'
        end
        object edtSM2FileSign: TEdit
          Left = 240
          Top = 32
          Width = 537
          Height = 21
          TabOrder = 1
        end
        object btnSignBrowse: TButton
          Left = 792
          Top = 32
          Width = 75
          Height = 25
          Caption = 'Browse'
          TabOrder = 2
          OnClick = btnSignBrowseClick
        end
        object mmoSignResult: TMemo
          Left = 16
          Top = 104
          Width = 857
          Height = 113
          TabOrder = 3
        end
        object btnSM2Verify: TButton
          Left = 536
          Top = 232
          Width = 129
          Height = 25
          Caption = 'Verify using Public Key'
          TabOrder = 6
          OnClick = btnSM2VerifyClick
        end
        object btnSM2SignFile: TButton
          Left = 240
          Top = 232
          Width = 281
          Height = 25
          Caption = 'Sign using Private Key and Public Key'
          TabOrder = 5
          OnClick = btnSM2SignFileClick
        end
        object btnSignFile: TButton
          Left = 704
          Top = 232
          Width = 75
          Height = 25
          Caption = 'Sign File'
          TabOrder = 7
          OnClick = btnSignFileClick
        end
        object btnVerifyFile: TButton
          Left = 800
          Top = 232
          Width = 75
          Height = 25
          Caption = 'Verify File'
          TabOrder = 8
          OnClick = btnVerifyFileClick
        end
        object btnSm2SignTime: TButton
          Left = 240
          Top = 272
          Width = 281
          Height = 25
          Caption = 'Sign Time'
          TabOrder = 9
          OnClick = btnSm2SignTimeClick
        end
        object btnSM2VerifyTime: TButton
          Left = 536
          Top = 272
          Width = 129
          Height = 25
          Caption = 'Verify Time'
          TabOrder = 10
          OnClick = btnSM2VerifyTimeClick
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
        Height = 403
        Caption = 'Key Exchange'
        TabOrder = 0
        object bvl6: TBevel
          Left = 152
          Top = 16
          Width = 17
          Height = 50
          Shape = bsLeftLine
        end
        object lblAId: TLabel
          Left = 168
          Top = 36
          Width = 47
          Height = 13
          Caption = 'A User Id:'
        end
        object lblBUserId: TLabel
          Left = 680
          Top = 36
          Width = 47
          Height = 13
          Caption = 'B User Id:'
        end
        object lblBSM2PublicKey: TLabel
          Left = 16
          Top = 84
          Width = 116
          Height = 13
          Caption = 'B SM2 Public Key (Hex):'
        end
        object lblBSm2PrivateKey: TLabel
          Left = 16
          Top = 116
          Width = 120
          Height = 13
          Caption = 'B SM2 Private Key (Hex):'
        end
        object lbl1: TLabel
          Left = 368
          Top = 36
          Width = 99
          Height = 13
          Caption = 'A SM2 Key is above.'
        end
        object btnSM2KeyExchange: TButton
          Left = 16
          Top = 32
          Width = 113
          Height = 25
          Caption = 'Sm2 Example 256'
          TabOrder = 0
          OnClick = btnSM2KeyExchangeClick
        end
        object edtSM2AUserId: TEdit
          Left = 232
          Top = 32
          Width = 121
          Height = 21
          TabOrder = 1
          Text = 'CnPack Team'
        end
        object edtSM2BUserId: TEdit
          Left = 744
          Top = 32
          Width = 121
          Height = 21
          TabOrder = 2
          Text = 'A Good User'
        end
        object edtSM2BPrivateKey: TEdit
          Left = 136
          Top = 112
          Width = 729
          Height = 21
          TabOrder = 4
          Text = '9751600494B8DCFD75ED60DD24C1A0C106E42BD52AE8C7B79A9456F10CFEBE9E'
        end
        object edtSM2BPublicKey: TEdit
          Left = 136
          Top = 80
          Width = 729
          Height = 21
          TabOrder = 3
          Text = 
            '0487A5DF8AA17DA358519587F88F1F21877D5F52B3A49E2EC7D9DAA76C3E94BD' +
            '19AC809BB986DDBBE6B7162BCD8FC5E1886D2DBEDC3A617421B2F213C4AB6C87' +
            '25'
        end
        object btnSM2ABKeyExchange: TButton
          Left = 136
          Top = 152
          Width = 281
          Height = 25
          Caption = 'Key Exchange A and B'
          TabOrder = 6
          OnClick = btnSM2ABKeyExchangeClick
        end
        object btnLoadSM2BKey: TButton
          Left = 760
          Top = 144
          Width = 105
          Height = 25
          Caption = 'Load SM2 Key'
          TabOrder = 5
          OnClick = btnLoadSM2BKeyClick
        end
      end
    end
    object tsSchnorr: TTabSheet
      Caption = 'Schnorr'
      ImageIndex = 3
      object grpSchnorr: TGroupBox
        Left = 8
        Top = 8
        Width = 889
        Height = 401
        Caption = 'Schnorr'
        TabOrder = 0
        object lblSchnorrProveCheckR: TLabel
          Left = 112
          Top = 28
          Width = 11
          Height = 13
          Caption = 'R:'
        end
        object lblSchnorrProveCheckZ: TLabel
          Left = 112
          Top = 60
          Width = 10
          Height = 13
          Caption = 'Z:'
        end
        object btnSchnorrProve: TButton
          Left = 16
          Top = 24
          Width = 75
          Height = 25
          Caption = 'Prove'
          TabOrder = 0
          OnClick = btnSchnorrProveClick
        end
        object edtSchnorrProveCheckR: TEdit
          Left = 136
          Top = 24
          Width = 729
          Height = 21
          TabOrder = 1
        end
        object edtSchnorrProveCheckZ: TEdit
          Left = 136
          Top = 56
          Width = 729
          Height = 21
          TabOrder = 3
        end
        object btnSchnorrCheck: TButton
          Left = 16
          Top = 52
          Width = 75
          Height = 25
          Caption = 'Check'
          TabOrder = 2
          OnClick = btnSchnorrCheckClick
        end
      end
    end
    object tsCollaborative: TTabSheet
      Caption = 'Collaborative'
      ImageIndex = 4
      object grpCollaborative: TGroupBox
        Left = 8
        Top = 8
        Width = 889
        Height = 401
        Caption = 'Collaborative'
        TabOrder = 0
        object lblSM2PrivateKeyA: TLabel
          Left = 16
          Top = 28
          Width = 98
          Height = 13
          Caption = 'SM2 Private Key (A):'
        end
        object lblSM2PrivateKeyB: TLabel
          Left = 16
          Top = 60
          Width = 98
          Height = 13
          Caption = 'SM2 Private Key (B):'
        end
        object lblSM2PublicKeyAB: TLabel
          Left = 16
          Top = 92
          Width = 78
          Height = 13
          Caption = 'SM2 Public Key:'
        end
        object lblCollId: TLabel
          Left = 16
          Top = 140
          Width = 85
          Height = 13
          Caption = 'Signature User Id:'
        end
        object lblSM2CollFileSign: TLabel
          Left = 264
          Top = 140
          Width = 68
          Height = 13
          Caption = 'File to Handle:'
        end
        object bvl7: TBevel
          Left = 16
          Top = 120
          Width = 857
          Height = 9
          Shape = bsTopLine
        end
        object bvl8: TBevel
          Left = 16
          Top = 312
          Width = 857
          Height = 9
          Shape = bsTopLine
        end
        object lblSM2CollText: TLabel
          Left = 16
          Top = 332
          Width = 79
          Height = 13
          Caption = 'Text To Encrypt:'
        end
        object edtSM2PrivateKeyA: TEdit
          Left = 128
          Top = 24
          Width = 673
          Height = 21
          TabOrder = 0
        end
        object edtSM2PrivateKeyB: TEdit
          Left = 128
          Top = 56
          Width = 673
          Height = 21
          TabOrder = 2
        end
        object edtSM2PublicKeyAB: TEdit
          Left = 128
          Top = 88
          Width = 673
          Height = 21
          TabOrder = 3
        end
        object btnSM2CollaborativeGen: TButton
          Left = 816
          Top = 24
          Width = 57
          Height = 81
          Caption = 'Generate'
          TabOrder = 1
          OnClick = btnSM2CollaborativeGenClick
        end
        object edtSM2CollUserId: TEdit
          Left = 128
          Top = 136
          Width = 121
          Height = 21
          TabOrder = 4
          Text = 'CnPack Team'
        end
        object edtSM2CollFileSign: TEdit
          Left = 336
          Top = 136
          Width = 465
          Height = 21
          TabOrder = 5
        end
        object btnCollSignBrowse: TButton
          Left = 816
          Top = 136
          Width = 57
          Height = 25
          Caption = 'Browse'
          TabOrder = 6
          OnClick = btnCollSignBrowseClick
        end
        object btnSM2CollSignFile: TButton
          Left = 16
          Top = 272
          Width = 281
          Height = 25
          Caption = 'Sign using 2 Private Keys and Public Key'
          TabOrder = 8
          OnClick = btnSM2CollSignFileClick
        end
        object btnSM2CollVerify: TButton
          Left = 744
          Top = 272
          Width = 129
          Height = 25
          Caption = 'Verify using Public Key'
          TabOrder = 9
          OnClick = btnSM2CollVerifyClick
        end
        object mmoSM2CollResult: TMemo
          Left = 16
          Top = 176
          Width = 857
          Height = 81
          TabOrder = 7
        end
        object chkCollPrefixByte: TCheckBox
          Left = 688
          Top = 332
          Width = 49
          Height = 17
          Caption = '$04'
          Checked = True
          State = cbChecked
          TabOrder = 12
        end
        object edtSM2CollText: TEdit
          Left = 104
          Top = 328
          Width = 577
          Height = 21
          TabOrder = 10
          Text = '1234567890'
        end
        object btnSM2CollEncrypt: TButton
          Left = 744
          Top = 328
          Width = 129
          Height = 25
          Caption = 'Encrypt using Public Key'
          TabOrder = 11
          OnClick = btnSM2CollEncryptClick
        end
        object btnSM2CollDecrypt: TButton
          Left = 104
          Top = 360
          Width = 217
          Height = 25
          Caption = 'Decrypt using 2 Private Keys'
          TabOrder = 13
          OnClick = btnSM2CollDecryptClick
        end
        object rbCollC1C2C3: TRadioButton
          Left = 616
          Top = 360
          Width = 65
          Height = 17
          Caption = 'C1C2C3'
          TabOrder = 15
        end
        object rbCollC1C3C2: TRadioButton
          Left = 536
          Top = 360
          Width = 65
          Height = 17
          Caption = 'C1C3C2'
          Checked = True
          TabOrder = 14
          TabStop = True
        end
      end
    end
    object tsTest: TTabSheet
      Caption = 'Other Test'
      ImageIndex = 5
      object btnSM2CreateMatrix: TButton
        Left = 16
        Top = 16
        Width = 193
        Height = 25
        Caption = 'Test Create Matrix  and Mul Point'
        TabOrder = 0
        OnClick = btnSM2CreateMatrixClick
      end
    end
  end
  object edtSM2PublicKey: TEdit
    Left = 136
    Top = 16
    Width = 673
    Height = 21
    TabOrder = 0
    Text = 
      '04450C442ADA3727DA5C61BED92AF9190B0C76F87473909DD573B7C46727CB88' +
      '066630371AE32EF6CDE503E9AFD0EA9CD762B543E9F1A1733A0EA1D66C970C72' +
      '1E'
  end
  object edtSM2PrivateKey: TEdit
    Left = 136
    Top = 48
    Width = 673
    Height = 21
    TabOrder = 2
    Text = 'F0D75B33B825F32B39A0AA3E143E4AE0210CB23BBFAADB006211C5053E2399A0'
  end
  object btnGenerateKey: TButton
    Left = 824
    Top = 16
    Width = 105
    Height = 25
    Caption = 'Generate Key'
    TabOrder = 1
    OnClick = btnGenerateKeyClick
  end
  object btnLoadSM2Key: TButton
    Left = 824
    Top = 48
    Width = 105
    Height = 25
    Caption = 'Load SM2 Key'
    TabOrder = 3
    OnClick = btnLoadSM2KeyClick
  end
  object btnVerifySm2Key: TButton
    Left = 824
    Top = 80
    Width = 105
    Height = 25
    Caption = 'Verify SM2 Key'
    TabOrder = 4
    OnClick = btnVerifySm2KeyClick
  end
  object dlgOpen1: TOpenDialog
    Left = 876
    Top = 136
  end
  object dlgSave1: TSaveDialog
    Left = 228
    Top = 432
  end
end
