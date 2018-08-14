object FormCrypt: TFormCrypt
  Left = 220
  Top = 115
  BorderStyle = bsDialog
  Caption = 'Crypt/Decrypt DEMO'
  ClientHeight = 433
  ClientWidth = 521
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 24
    Top = 24
    Width = 473
    Height = 385
    ActivePage = tsBase64
    MultiLine = True
    TabOrder = 0
    object tsDES: TTabSheet
      Caption = 'DES'
      object grpdES: TGroupBox
        Left = 24
        Top = 24
        Width = 417
        Height = 289
        Anchors = [akLeft, akTop, akRight]
        Caption = 'DES'
        TabOrder = 0
        object lbl1: TLabel
          Left = 24
          Top = 36
          Width = 28
          Height = 13
          Caption = 'From:'
        end
        object lblKey: TLabel
          Left = 24
          Top = 72
          Width = 22
          Height = 13
          Caption = 'Key:'
        end
        object lblCode: TLabel
          Left = 24
          Top = 164
          Width = 29
          Height = 13
          Caption = 'Code:'
        end
        object lblOrigin: TLabel
          Left = 24
          Top = 248
          Width = 32
          Height = 13
          Caption = 'Origin:'
        end
        object edtDesFrom: TEdit
          Left = 72
          Top = 32
          Width = 169
          Height = 21
          TabOrder = 0
          Text = 'Sample Text'
        end
        object edtKey: TEdit
          Left = 72
          Top = 72
          Width = 169
          Height = 21
          TabOrder = 1
          Text = '123456'
        end
        object btnDesCrypt: TButton
          Left = 72
          Top = 112
          Width = 75
          Height = 25
          Caption = 'DES Crypt'
          TabOrder = 2
          OnClick = btnDesCryptClick
        end
        object edtCode: TEdit
          Left = 72
          Top = 160
          Width = 169
          Height = 21
          TabOrder = 3
        end
        object btnDesDecrypt: TButton
          Left = 72
          Top = 200
          Width = 75
          Height = 25
          Caption = 'DES Decrypt'
          TabOrder = 4
          OnClick = btnDesDecryptClick
        end
        object edtOrigin: TEdit
          Left = 72
          Top = 244
          Width = 169
          Height = 21
          TabOrder = 5
        end
      end
    end
    object tsMD5: TTabSheet
      Caption = 'MD5'
      ImageIndex = 1
      object grpMd5: TGroupBox
        Left = 24
        Top = 24
        Width = 417
        Height = 289
        Anchors = [akLeft, akTop, akRight]
        Caption = 'MD5'
        TabOrder = 0
        object lblfROM: TLabel
          Left = 24
          Top = 44
          Width = 28
          Height = 13
          Caption = 'From:'
        end
        object lblMD5HmacKey: TLabel
          Left = 264
          Top = 44
          Width = 51
          Height = 13
          Caption = 'Hmac Key:'
        end
        object edtMD5: TEdit
          Left = 72
          Top = 40
          Width = 169
          Height = 21
          TabOrder = 0
          Text = 'Sample Text'
        end
        object btnMd5: TButton
          Left = 72
          Top = 80
          Width = 75
          Height = 25
          Caption = 'MD5'
          TabOrder = 2
          OnClick = btnMd5Click
        end
        object pnlMd5: TPanel
          Left = 24
          Top = 136
          Width = 369
          Height = 41
          BevelOuter = bvNone
          TabOrder = 5
          OnDblClick = ResultDblClick
        end
        object btnMd5File: TButton
          Left = 72
          Top = 208
          Width = 75
          Height = 25
          Caption = 'File MD5'
          TabOrder = 6
          OnClick = btnMd5FileClick
        end
        object edtMD5HmacKey: TEdit
          Left = 320
          Top = 40
          Width = 73
          Height = 21
          TabOrder = 1
          Text = 'HmacKey'
        end
        object btnMD5Hmac: TButton
          Left = 320
          Top = 80
          Width = 75
          Height = 25
          Caption = 'MD5 Hmac'
          TabOrder = 4
          OnClick = btnMD5HmacClick
        end
        object btnUMd5: TButton
          Left = 168
          Top = 80
          Width = 75
          Height = 25
          Caption = 'Utf16 MD5'
          TabOrder = 3
          OnClick = btnUMd5Click
        end
      end
    end
    object tsBase64: TTabSheet
      Caption = 'Base64'
      ImageIndex = 2
      object GroupBox1: TGroupBox
        Left = 24
        Top = 24
        Width = 417
        Height = 289
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Base64'
        TabOrder = 0
        object lbl2: TLabel
          Left = 24
          Top = 36
          Width = 28
          Height = 13
          Caption = 'From:'
        end
        object lbl3: TLabel
          Left = 24
          Top = 144
          Width = 29
          Height = 13
          Caption = 'Code:'
        end
        object lbl4: TLabel
          Left = 24
          Top = 248
          Width = 32
          Height = 13
          Caption = 'Origin:'
        end
        object edtBase64from: TEdit
          Left = 72
          Top = 32
          Width = 169
          Height = 21
          TabOrder = 0
          Text = 'Sample Text'
        end
        object Button1: TButton
          Left = 72
          Top = 84
          Width = 105
          Height = 25
          Caption = 'Base64 Encode'
          TabOrder = 1
          OnClick = Button1Click
        end
        object edtBase64Result: TEdit
          Left = 72
          Top = 140
          Width = 169
          Height = 21
          TabOrder = 3
        end
        object btnBase64Decode: TButton
          Left = 72
          Top = 190
          Width = 105
          Height = 25
          Caption = 'Base64 Decode'
          TabOrder = 4
          OnClick = btnBase64DecodeClick
        end
        object edtBase64Decode: TEdit
          Left = 72
          Top = 244
          Width = 169
          Height = 21
          TabOrder = 6
        end
        object btnBase64File: TButton
          Left = 288
          Top = 136
          Width = 75
          Height = 25
          Caption = 'File Base64'
          TabOrder = 2
          OnClick = btnBase64FileClick
        end
        object btnDeBase64File: TButton
          Left = 240
          Top = 190
          Width = 121
          Height = 25
          Caption = 'Base64 Decode to File'
          TabOrder = 5
          OnClick = btnDeBase64FileClick
        end
      end
    end
    object tsCRC32: TTabSheet
      Caption = 'CRC32'
      ImageIndex = 3
      object grpCRC32: TGroupBox
        Left = 24
        Top = 24
        Width = 417
        Height = 289
        Anchors = [akLeft, akTop, akRight]
        Caption = 'CRC32'
        TabOrder = 0
        object lblCRC: TLabel
          Left = 24
          Top = 44
          Width = 28
          Height = 13
          Caption = 'From:'
        end
        object lblCRC32HmacKey: TLabel
          Left = 264
          Top = 44
          Width = 51
          Height = 13
          Caption = 'Hmac Key:'
        end
        object edtCRC32: TEdit
          Left = 72
          Top = 40
          Width = 169
          Height = 21
          TabOrder = 0
          Text = 'Sample Text'
        end
        object btnCRC32: TButton
          Left = 72
          Top = 80
          Width = 75
          Height = 25
          Caption = 'CRC32'
          TabOrder = 2
          OnClick = btnCRC32Click
        end
        object pnlCRC32: TPanel
          Left = 24
          Top = 144
          Width = 369
          Height = 41
          BevelOuter = bvNone
          TabOrder = 4
          OnDblClick = ResultDblClick
        end
        object btnFileCRC32: TButton
          Left = 72
          Top = 208
          Width = 75
          Height = 25
          Caption = 'File CRC32'
          TabOrder = 5
          OnClick = btnFileCRC32Click
        end
        object edtCRC32HmacKey: TEdit
          Left = 320
          Top = 40
          Width = 73
          Height = 21
          TabOrder = 1
          Text = 'HmacKey'
        end
        object btnCRC32Hmac: TButton
          Left = 320
          Top = 80
          Width = 75
          Height = 25
          Caption = 'CRC32 Hmac'
          TabOrder = 3
          OnClick = btnCRC32HmacClick
        end
      end
    end
    object tsCRC64: TTabSheet
      Caption = 'CRC64'
      ImageIndex = 4
      object grp1: TGroupBox
        Left = 24
        Top = 24
        Width = 417
        Height = 289
        Anchors = [akLeft, akTop, akRight]
        Caption = 'CRC64'
        TabOrder = 0
        object lbl5: TLabel
          Left = 24
          Top = 44
          Width = 28
          Height = 13
          Caption = 'From:'
        end
        object lblCRC64HmacKey: TLabel
          Left = 264
          Top = 44
          Width = 51
          Height = 13
          Caption = 'Hmac Key:'
        end
        object edtCRC64: TEdit
          Left = 72
          Top = 40
          Width = 169
          Height = 21
          TabOrder = 0
          Text = 'Sample Text'
        end
        object btnCRC64: TButton
          Left = 72
          Top = 80
          Width = 75
          Height = 25
          Caption = 'CRC64'
          TabOrder = 2
          OnClick = btnCRC64Click
        end
        object pnlCRC64: TPanel
          Left = 24
          Top = 144
          Width = 233
          Height = 41
          BevelOuter = bvNone
          TabOrder = 4
          OnDblClick = ResultDblClick
        end
        object btnFileCRC64: TButton
          Left = 72
          Top = 208
          Width = 75
          Height = 25
          Caption = 'File CRC64'
          TabOrder = 5
          OnClick = btnFileCRC64Click
        end
        object btnCRC64Hmac: TButton
          Left = 320
          Top = 80
          Width = 75
          Height = 25
          Caption = 'CRC64 Hmac'
          TabOrder = 3
          OnClick = btnCRC64HmacClick
        end
        object edtCRC64HmacKey: TEdit
          Left = 320
          Top = 40
          Width = 73
          Height = 21
          TabOrder = 1
          Text = 'HmacKey'
        end
      end
    end
    object tsSha1: TTabSheet
      Caption = 'SHA1'
      ImageIndex = 5
      object grpSha1: TGroupBox
        Left = 24
        Top = 24
        Width = 417
        Height = 289
        Anchors = [akLeft, akTop, akRight]
        Caption = 'SHA1'
        TabOrder = 0
        object lblSha1: TLabel
          Left = 24
          Top = 44
          Width = 28
          Height = 13
          Caption = 'From:'
        end
        object lblSHA1HmacKey: TLabel
          Left = 264
          Top = 44
          Width = 51
          Height = 13
          Caption = 'Hmac Key:'
        end
        object edtSha1: TEdit
          Left = 72
          Top = 40
          Width = 169
          Height = 21
          TabOrder = 0
          Text = 'Sample Text'
        end
        object btnSha1: TButton
          Left = 72
          Top = 80
          Width = 75
          Height = 25
          Caption = 'SHA1'
          TabOrder = 2
          OnClick = btnSha1Click
        end
        object pnlSha1: TPanel
          Left = 8
          Top = 136
          Width = 401
          Height = 41
          BevelOuter = bvNone
          TabOrder = 5
          OnDblClick = ResultDblClick
        end
        object btnFileSha1: TButton
          Left = 72
          Top = 208
          Width = 75
          Height = 25
          Caption = 'File SHA1'
          TabOrder = 6
          OnClick = btnFileSha1Click
        end
        object edtSHA1HMacKey: TEdit
          Left = 320
          Top = 40
          Width = 73
          Height = 21
          TabOrder = 1
          Text = 'HmacKey'
        end
        object btnSHA1Hmac: TButton
          Left = 320
          Top = 80
          Width = 75
          Height = 25
          Caption = 'SHA1 Hmac'
          TabOrder = 4
          OnClick = btnSHA1HmacClick
        end
        object btnUSHA1: TButton
          Left = 168
          Top = 80
          Width = 75
          Height = 25
          Caption = 'Utf16 SHA1'
          TabOrder = 3
          OnClick = btnUSHA1Click
        end
      end
    end
    object tsSM3: TTabSheet
      Caption = 'SM3'
      ImageIndex = 6
      object grpSM3: TGroupBox
        Left = 24
        Top = 24
        Width = 417
        Height = 289
        Anchors = [akLeft, akTop, akRight]
        Caption = 'SM3'
        TabOrder = 0
        object lblSM3: TLabel
          Left = 24
          Top = 44
          Width = 28
          Height = 13
          Caption = 'From:'
        end
        object lblSm3Result: TLabel
          Left = 16
          Top = 136
          Width = 393
          Height = 41
          Alignment = taCenter
          AutoSize = False
          Layout = tlCenter
          OnDblClick = ResultDblClick
        end
        object lblSM3HmacKey: TLabel
          Left = 264
          Top = 44
          Width = 51
          Height = 13
          Caption = 'Hmac Key:'
        end
        object edtSM3: TEdit
          Left = 72
          Top = 40
          Width = 169
          Height = 21
          TabOrder = 0
          Text = 'Sample Text'
        end
        object btnSM3: TButton
          Left = 72
          Top = 80
          Width = 75
          Height = 25
          Caption = 'SM3'
          TabOrder = 2
          OnClick = btnSM3Click
        end
        object btnFileSM3: TButton
          Left = 72
          Top = 208
          Width = 75
          Height = 25
          Caption = 'File SM3'
          TabOrder = 5
          OnClick = btnFileSM3Click
        end
        object edtSM3HMacKey: TEdit
          Left = 320
          Top = 40
          Width = 73
          Height = 21
          TabOrder = 1
          Text = 'HmacKey'
        end
        object btnSM3Hmac: TButton
          Left = 320
          Top = 80
          Width = 75
          Height = 25
          Caption = 'SM3 Hmac'
          TabOrder = 4
          OnClick = btnSM3HmacClick
        end
        object btnUSM3: TButton
          Left = 168
          Top = 80
          Width = 75
          Height = 25
          Caption = 'Utf16 SM3'
          TabOrder = 3
          OnClick = btnUSM3Click
        end
      end
    end
    object tsSM4: TTabSheet
      Caption = 'SM4'
      ImageIndex = 7
      object grpSM4: TGroupBox
        Left = 24
        Top = 24
        Width = 417
        Height = 289
        Anchors = [akLeft, akTop, akRight]
        Caption = 'SM3'
        TabOrder = 0
        object lblSm4: TLabel
          Left = 24
          Top = 36
          Width = 28
          Height = 13
          Caption = 'From:'
        end
        object lblSm4Key: TLabel
          Left = 24
          Top = 72
          Width = 22
          Height = 13
          Caption = 'Key:'
        end
        object lblSm4Dec: TLabel
          Left = 24
          Top = 248
          Width = 32
          Height = 13
          Caption = 'Origin:'
        end
        object lblSm4Code: TLabel
          Left = 24
          Top = 164
          Width = 29
          Height = 13
          Caption = 'Code:'
        end
        object edtSm4: TEdit
          Left = 72
          Top = 32
          Width = 169
          Height = 21
          TabOrder = 0
          Text = 'Sample Text'
        end
        object btnSm4: TButton
          Left = 72
          Top = 112
          Width = 75
          Height = 25
          Caption = 'SM4 Encrypt'
          TabOrder = 2
          OnClick = btnSm4Click
        end
        object edtSm4Key: TEdit
          Left = 72
          Top = 72
          Width = 169
          Height = 21
          TabOrder = 1
          Text = '123456'
        end
        object edtSm4Dec: TEdit
          Left = 72
          Top = 244
          Width = 169
          Height = 21
          TabOrder = 7
        end
        object btnSm4Dec: TButton
          Left = 72
          Top = 200
          Width = 75
          Height = 25
          Caption = 'SM4 Decrypt'
          TabOrder = 6
          OnClick = btnSm4DecClick
        end
        object edtSm4Code: TEdit
          Left = 72
          Top = 160
          Width = 169
          Height = 21
          TabOrder = 5
        end
        object rbSm4Ecb: TRadioButton
          Left = 168
          Top = 116
          Width = 57
          Height = 17
          Caption = 'ECB'
          TabOrder = 3
        end
        object rbSm4CBC: TRadioButton
          Left = 232
          Top = 116
          Width = 57
          Height = 17
          Caption = 'CBC'
          Checked = True
          TabOrder = 4
          TabStop = True
        end
      end
    end
    object tsAES: TTabSheet
      Caption = 'AES'
      ImageIndex = 8
      object grpAes: TGroupBox
        Left = 24
        Top = 24
        Width = 417
        Height = 289
        Anchors = [akLeft, akTop, akRight]
        Caption = 'AES'
        TabOrder = 0
        object lblAesFrom: TLabel
          Left = 24
          Top = 36
          Width = 28
          Height = 13
          Caption = 'From:'
        end
        object lblAesKey: TLabel
          Left = 24
          Top = 66
          Width = 22
          Height = 13
          Caption = 'Key:'
        end
        object lblAesOrigin: TLabel
          Left = 24
          Top = 248
          Width = 32
          Height = 13
          Caption = 'Origin:'
        end
        object lblAesCode: TLabel
          Left = 24
          Top = 180
          Width = 29
          Height = 13
          Caption = 'Code:'
        end
        object lblKeyBit: TLabel
          Left = 24
          Top = 98
          Width = 34
          Height = 13
          Caption = 'KeyBit:'
        end
        object edtAes: TEdit
          Left = 72
          Top = 32
          Width = 169
          Height = 21
          TabOrder = 0
          Text = 'Sample Text'
        end
        object btnAesEncrypt: TButton
          Left = 72
          Top = 128
          Width = 75
          Height = 25
          Caption = 'AES Encrypt'
          TabOrder = 3
          OnClick = btnAesEncryptClick
        end
        object edtAesKey: TEdit
          Left = 72
          Top = 64
          Width = 169
          Height = 21
          TabOrder = 1
          Text = '123456'
        end
        object edtAesDecrypt: TEdit
          Left = 72
          Top = 244
          Width = 169
          Height = 21
          TabOrder = 8
        end
        object btnAesDecrypt: TButton
          Left = 72
          Top = 208
          Width = 75
          Height = 25
          Caption = 'AES Decrypt'
          TabOrder = 7
          OnClick = btnAesDecryptClick
        end
        object edtAesResult: TEdit
          Left = 72
          Top = 176
          Width = 169
          Height = 21
          TabOrder = 6
        end
        object rbAesecb: TRadioButton
          Left = 168
          Top = 132
          Width = 57
          Height = 17
          Caption = 'ECB'
          TabOrder = 4
        end
        object rbAescbc: TRadioButton
          Left = 232
          Top = 132
          Width = 57
          Height = 17
          Caption = 'CBC'
          Checked = True
          TabOrder = 5
          TabStop = True
        end
        object cbbAesKeyBitType: TComboBox
          Left = 72
          Top = 96
          Width = 169
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 2
          Items.Strings = (
            '128 Bit'
            '192 Bit'
            '256 Bit')
        end
      end
    end
    object tsSHA224: TTabSheet
      Caption = 'SHA224'
      ImageIndex = 10
      object grpSHA224: TGroupBox
        Left = 24
        Top = 24
        Width = 417
        Height = 289
        Anchors = [akLeft, akTop, akRight]
        Caption = 'SHA224'
        TabOrder = 0
        object lblSHA224: TLabel
          Left = 24
          Top = 44
          Width = 28
          Height = 13
          Caption = 'From:'
        end
        object lblSHA224HmacKey: TLabel
          Left = 264
          Top = 44
          Width = 51
          Height = 13
          Caption = 'Hmac Key:'
        end
        object edtSHA224: TEdit
          Left = 72
          Top = 40
          Width = 169
          Height = 21
          TabOrder = 0
          Text = 'Sample Text'
        end
        object btnSHA224: TButton
          Left = 72
          Top = 80
          Width = 75
          Height = 25
          Caption = 'SHA224'
          TabOrder = 2
          OnClick = btnSHA224Click
        end
        object pnlSHA224: TPanel
          Left = 8
          Top = 136
          Width = 401
          Height = 41
          BevelOuter = bvNone
          TabOrder = 5
          OnDblClick = ResultDblClick
        end
        object btnSHA224File: TButton
          Left = 72
          Top = 208
          Width = 75
          Height = 25
          Caption = 'File SHA224'
          TabOrder = 6
          OnClick = btnSHA224FileClick
        end
        object edtSHA224HmacKey: TEdit
          Left = 320
          Top = 40
          Width = 73
          Height = 21
          TabOrder = 1
          Text = 'HmacKey'
        end
        object btnSHA224Hmac: TButton
          Left = 320
          Top = 80
          Width = 75
          Height = 25
          Caption = 'SHA224 Hmac'
          TabOrder = 4
          OnClick = btnSHA224HmacClick
        end
        object btnUSHA224: TButton
          Left = 168
          Top = 80
          Width = 75
          Height = 25
          Caption = 'Utf16 SHA224'
          TabOrder = 3
          OnClick = btnUSHA224Click
        end
      end
    end
    object tsSHA256: TTabSheet
      Caption = 'SHA256'
      ImageIndex = 9
      object grpSHA256: TGroupBox
        Left = 24
        Top = 24
        Width = 417
        Height = 289
        Anchors = [akLeft, akTop, akRight]
        Caption = 'SHA256'
        TabOrder = 0
        object lblSHA256: TLabel
          Left = 24
          Top = 44
          Width = 28
          Height = 13
          Caption = 'From:'
        end
        object lblSHA256HmacKey: TLabel
          Left = 264
          Top = 44
          Width = 51
          Height = 13
          Caption = 'Hmac Key:'
        end
        object edtSHA256: TEdit
          Left = 72
          Top = 40
          Width = 169
          Height = 21
          TabOrder = 0
          Text = 'Sample Text'
        end
        object btnSHA256: TButton
          Left = 72
          Top = 80
          Width = 75
          Height = 25
          Caption = 'SHA256'
          TabOrder = 2
          OnClick = btnSHA256Click
        end
        object pnlSHA256: TPanel
          Left = 8
          Top = 136
          Width = 401
          Height = 41
          BevelOuter = bvNone
          TabOrder = 5
          OnDblClick = ResultDblClick
        end
        object btnFileSHA256: TButton
          Left = 72
          Top = 208
          Width = 75
          Height = 25
          Caption = 'File SHA256'
          TabOrder = 6
          OnClick = btnFileSHA256Click
        end
        object edtSHA256HmacKey: TEdit
          Left = 320
          Top = 40
          Width = 73
          Height = 21
          TabOrder = 1
          Text = 'HmacKey'
        end
        object btnSHA256Hmac: TButton
          Left = 320
          Top = 80
          Width = 75
          Height = 25
          Caption = 'SHA256 Hmac'
          TabOrder = 4
          OnClick = btnSHA256HmacClick
        end
        object btnUSHA256: TButton
          Left = 168
          Top = 80
          Width = 75
          Height = 25
          Caption = 'Utf16 SHA256'
          TabOrder = 3
          OnClick = btnUSHA256Click
        end
      end
    end
    object tsSHA384: TTabSheet
      Caption = 'SHA384'
      ImageIndex = 12
      object grpSHA384: TGroupBox
        Left = 24
        Top = 24
        Width = 417
        Height = 289
        Anchors = [akLeft, akTop, akRight]
        Caption = 'SHA384'
        TabOrder = 0
        object lblSHA384: TLabel
          Left = 24
          Top = 44
          Width = 28
          Height = 13
          Caption = 'From:'
        end
        object lblSHA384Result: TLabel
          Left = 8
          Top = 136
          Width = 401
          Height = 41
          Alignment = taCenter
          AutoSize = False
          Layout = tlCenter
          OnDblClick = ResultDblClick
        end
        object lblSHA384HmacKey: TLabel
          Left = 264
          Top = 44
          Width = 51
          Height = 13
          Caption = 'Hmac Key:'
        end
        object edtSHA384: TEdit
          Left = 72
          Top = 40
          Width = 169
          Height = 21
          TabOrder = 0
          Text = 'Sample Text'
        end
        object btnSHA384: TButton
          Left = 72
          Top = 80
          Width = 75
          Height = 25
          Caption = 'SHA384'
          TabOrder = 2
          OnClick = btnSHA384Click
        end
        object btnSHA384File: TButton
          Left = 72
          Top = 208
          Width = 75
          Height = 25
          Caption = 'File SHA384'
          TabOrder = 5
          OnClick = btnSHA384FileClick
        end
        object edtSHA384HmacKey: TEdit
          Left = 320
          Top = 40
          Width = 73
          Height = 21
          TabOrder = 1
          Text = 'HmacKey'
        end
        object btnSHA384Hmac: TButton
          Left = 320
          Top = 80
          Width = 75
          Height = 25
          Caption = 'SHA384 Hmac'
          TabOrder = 4
          OnClick = btnSHA384HmacClick
        end
        object btnUSHA384: TButton
          Left = 168
          Top = 80
          Width = 75
          Height = 25
          Caption = 'Utf16 SHA384'
          TabOrder = 3
          OnClick = btnUSHA384Click
        end
      end
    end
    object tsSHA512: TTabSheet
      Caption = 'SHA512'
      ImageIndex = 11
      object grpSHA512: TGroupBox
        Left = 24
        Top = 24
        Width = 417
        Height = 289
        Anchors = [akLeft, akTop, akRight]
        Caption = 'SHA512'
        TabOrder = 0
        object lblSHA512: TLabel
          Left = 24
          Top = 44
          Width = 28
          Height = 13
          Caption = 'From:'
        end
        object lblSHA512Result: TLabel
          Left = 8
          Top = 136
          Width = 401
          Height = 41
          Alignment = taCenter
          AutoSize = False
          Layout = tlCenter
          OnDblClick = ResultDblClick
        end
        object lblSHA512HmacKey: TLabel
          Left = 264
          Top = 44
          Width = 51
          Height = 13
          Caption = 'Hmac Key:'
        end
        object edtSHA512: TEdit
          Left = 72
          Top = 40
          Width = 169
          Height = 21
          TabOrder = 0
          Text = 'Sample Text'
        end
        object btnSHA512: TButton
          Left = 72
          Top = 80
          Width = 75
          Height = 25
          Caption = 'SHA512'
          TabOrder = 2
          OnClick = btnSHA512Click
        end
        object btnSHA512File: TButton
          Left = 72
          Top = 208
          Width = 75
          Height = 25
          Caption = 'File SHA512'
          TabOrder = 5
          OnClick = btnSHA512FileClick
        end
        object edtSHA512HmacKey: TEdit
          Left = 320
          Top = 40
          Width = 73
          Height = 21
          TabOrder = 1
          Text = 'HmacKey'
        end
        object btnSHA512Hmac: TButton
          Left = 320
          Top = 80
          Width = 75
          Height = 25
          Caption = 'SHA512 Hmac'
          TabOrder = 4
          OnClick = btnSHA512HmacClick
        end
        object btnUSHA512: TButton
          Left = 168
          Top = 80
          Width = 75
          Height = 25
          Caption = 'Utf16 SHA512'
          TabOrder = 3
          OnClick = btnUSHA512Click
        end
      end
    end
    object tsSHA3_224: TTabSheet
      Caption = 'SHA3_224'
      ImageIndex = 10
      object grpSHA3_224: TGroupBox
        Left = 24
        Top = 24
        Width = 417
        Height = 289
        Anchors = [akLeft, akTop, akRight]
        Caption = 'SHA3_224'
        TabOrder = 0
        object lblSHA3_224: TLabel
          Left = 24
          Top = 44
          Width = 28
          Height = 13
          Caption = 'From:'
        end
        object lblSHA3_224HmacKey: TLabel
          Left = 264
          Top = 44
          Width = 51
          Height = 13
          Caption = 'Hmac Key:'
        end
        object edtSHA3_224: TEdit
          Left = 72
          Top = 40
          Width = 169
          Height = 21
          TabOrder = 0
          Text = 'Sample Text'
        end
        object btnSHA3_224: TButton
          Left = 72
          Top = 80
          Width = 75
          Height = 25
          Caption = 'SHA3_224'
          TabOrder = 2
          OnClick = btnSHA3_224Click
        end
        object pnlSHA3_224: TPanel
          Left = 8
          Top = 136
          Width = 401
          Height = 41
          BevelOuter = bvNone
          TabOrder = 5
          OnDblClick = ResultDblClick
        end
        object btnSHA3_224File: TButton
          Left = 72
          Top = 208
          Width = 75
          Height = 25
          Caption = 'File SHA3_224'
          TabOrder = 6
          OnClick = btnSHA3_224FileClick
        end
        object edtSHA3_224HmacKey: TEdit
          Left = 320
          Top = 40
          Width = 73
          Height = 21
          TabOrder = 1
          Text = 'HmacKey'
        end
        object btnSHA3_224Hmac: TButton
          Left = 304
          Top = 80
          Width = 91
          Height = 25
          Caption = 'SHA3_224 Hmac'
          TabOrder = 4
          OnClick = btnSHA3_224HmacClick
        end
        object btnUSHA3_224: TButton
          Left = 168
          Top = 80
          Width = 75
          Height = 25
          Caption = 'Utf16 SHA3_224'
          TabOrder = 3
          OnClick = btnUSHA3_224Click
        end
      end
    end
    object tsSHA3_256: TTabSheet
      Caption = 'SHA3_256'
      ImageIndex = 9
      object grpSHA3_256: TGroupBox
        Left = 24
        Top = 24
        Width = 417
        Height = 289
        Anchors = [akLeft, akTop, akRight]
        Caption = 'SHA3_256'
        TabOrder = 0
        object lblSHA3_256: TLabel
          Left = 24
          Top = 44
          Width = 28
          Height = 13
          Caption = 'From:'
        end
        object lblSHA3_256HmacKey: TLabel
          Left = 264
          Top = 44
          Width = 51
          Height = 13
          Caption = 'Hmac Key:'
        end
        object edtSHA3_256: TEdit
          Left = 72
          Top = 40
          Width = 169
          Height = 21
          TabOrder = 0
          Text = 'Sample Text'
        end
        object btnSHA3_256: TButton
          Left = 72
          Top = 80
          Width = 75
          Height = 25
          Caption = 'SHA3_256'
          TabOrder = 2
          OnClick = btnSHA3_256Click
        end
        object pnlSHA3_256: TPanel
          Left = 8
          Top = 136
          Width = 401
          Height = 41
          BevelOuter = bvNone
          TabOrder = 5
          OnDblClick = ResultDblClick
        end
        object btnFileSHA3_256: TButton
          Left = 72
          Top = 208
          Width = 75
          Height = 25
          Caption = 'File SHA3_256'
          TabOrder = 6
          OnClick = btnFileSHA3_256Click
        end
        object edtSHA3_256HmacKey: TEdit
          Left = 320
          Top = 40
          Width = 73
          Height = 21
          TabOrder = 1
          Text = 'HmacKey'
        end
        object btnSHA3_256Hmac: TButton
          Left = 304
          Top = 80
          Width = 91
          Height = 25
          Caption = 'SHA3_256 Hmac'
          TabOrder = 4
          OnClick = btnSHA3_256HmacClick
        end
        object btnUSHA3_256: TButton
          Left = 168
          Top = 80
          Width = 75
          Height = 25
          Caption = 'Utf16 SHA3_256'
          TabOrder = 3
          OnClick = btnUSHA3_256Click
        end
      end
    end
    object tsSHA3_384: TTabSheet
      Caption = 'SHA3_384'
      ImageIndex = 12
      object grpSHA3_384: TGroupBox
        Left = 24
        Top = 24
        Width = 417
        Height = 289
        Anchors = [akLeft, akTop, akRight]
        Caption = 'SHA3_384'
        TabOrder = 0
        object lblSHA3_384: TLabel
          Left = 24
          Top = 44
          Width = 28
          Height = 13
          Caption = 'From:'
        end
        object lblSHA3_384Result: TLabel
          Left = 8
          Top = 136
          Width = 401
          Height = 41
          Alignment = taCenter
          AutoSize = False
          Layout = tlCenter
          OnDblClick = ResultDblClick
        end
        object lblSHA3_384HmacKey: TLabel
          Left = 264
          Top = 44
          Width = 51
          Height = 13
          Caption = 'Hmac Key:'
        end
        object edtSHA3_384: TEdit
          Left = 72
          Top = 40
          Width = 169
          Height = 21
          TabOrder = 0
          Text = 'Sample Text'
        end
        object btnSHA3_384: TButton
          Left = 72
          Top = 80
          Width = 75
          Height = 25
          Caption = 'SHA3_384'
          TabOrder = 2
          OnClick = btnSHA3_384Click
        end
        object btnSHA3_384File: TButton
          Left = 72
          Top = 208
          Width = 75
          Height = 25
          Caption = 'File SHA3_384'
          TabOrder = 5
          OnClick = btnSHA3_384FileClick
        end
        object edtSHA3_384HmacKey: TEdit
          Left = 320
          Top = 40
          Width = 73
          Height = 21
          TabOrder = 1
          Text = 'HmacKey'
        end
        object btnSHA3_384Hmac: TButton
          Left = 304
          Top = 80
          Width = 91
          Height = 25
          Caption = 'SHA3_384 Hmac'
          TabOrder = 4
          OnClick = btnSHA3_384HmacClick
        end
        object btnUSHA3_384: TButton
          Left = 168
          Top = 80
          Width = 75
          Height = 25
          Caption = 'Utf16 SHA3_384'
          TabOrder = 3
          OnClick = btnUSHA3_384Click
        end
      end
    end
    object tsSHA3_512: TTabSheet
      Caption = 'SHA3_512'
      ImageIndex = 11
      object grpSHA3_512: TGroupBox
        Left = 24
        Top = 24
        Width = 417
        Height = 289
        Anchors = [akLeft, akTop, akRight]
        Caption = 'SHA3_512'
        TabOrder = 0
        object lblSHA3_512: TLabel
          Left = 24
          Top = 44
          Width = 28
          Height = 13
          Caption = 'From:'
        end
        object lblSHA3_512Result: TLabel
          Left = 8
          Top = 136
          Width = 401
          Height = 41
          Alignment = taCenter
          AutoSize = False
          Layout = tlCenter
          OnDblClick = ResultDblClick
        end
        object lblSHA3_512HmacKey: TLabel
          Left = 264
          Top = 44
          Width = 51
          Height = 13
          Caption = 'Hmac Key:'
        end
        object edtSHA3_512: TEdit
          Left = 72
          Top = 40
          Width = 169
          Height = 21
          TabOrder = 0
          Text = 'Sample Text'
        end
        object btnSHA3_512: TButton
          Left = 72
          Top = 80
          Width = 75
          Height = 25
          Caption = 'SHA3_512'
          TabOrder = 2
          OnClick = btnSHA3_512Click
        end
        object btnSHA3_512File: TButton
          Left = 72
          Top = 208
          Width = 75
          Height = 25
          Caption = 'File SHA3_512'
          TabOrder = 5
          OnClick = btnSHA3_512FileClick
        end
        object edtSHA3_512HmacKey: TEdit
          Left = 320
          Top = 40
          Width = 73
          Height = 21
          TabOrder = 1
          Text = 'HmacKey'
        end
        object btnSHA3_512Hmac: TButton
          Left = 304
          Top = 80
          Width = 91
          Height = 25
          Caption = 'SHA3_512 Hmac'
          TabOrder = 4
          OnClick = btnSHA3_512HmacClick
        end
        object btnUSHA3_512: TButton
          Left = 168
          Top = 80
          Width = 75
          Height = 25
          Caption = 'Utf16 SHA3_512'
          TabOrder = 3
          OnClick = btnUSHA3_512Click
        end
      end
    end
    object tsZUC: TTabSheet
      Caption = 'ZUC'
      ImageIndex = 13
      object grpZuc: TGroupBox
        Left = 24
        Top = 24
        Width = 417
        Height = 289
        Anchors = [akLeft, akTop, akRight]
        Caption = 'ZUC'
        TabOrder = 0
        object lblZuc1: TLabel
          Left = 8
          Top = 136
          Width = 401
          Height = 41
          Alignment = taCenter
          AutoSize = False
          Layout = tlCenter
          OnDblClick = ResultDblClick
        end
        object btnZUC1: TButton
          Left = 32
          Top = 32
          Width = 75
          Height = 25
          Caption = 'ZUC 00'
          TabOrder = 0
          OnClick = btnZUC1Click
        end
        object btnZUCEIA31: TButton
          Left = 288
          Top = 32
          Width = 75
          Height = 25
          Caption = 'ZUC EIA3 1'
          TabOrder = 2
          OnClick = btnZUCEIA31Click
        end
        object btnZUC2: TButton
          Left = 32
          Top = 72
          Width = 75
          Height = 25
          Caption = 'ZUC FF'
          TabOrder = 3
          OnClick = btnZUC2Click
        end
        object btnZUC3: TButton
          Left = 32
          Top = 112
          Width = 75
          Height = 25
          Caption = 'ZUC Rand'
          TabOrder = 5
          OnClick = btnZUC3Click
        end
        object btnZUC4: TButton
          Left = 32
          Top = 152
          Width = 75
          Height = 25
          Caption = 'ZUC 2000'
          TabOrder = 6
          OnClick = btnZUC4Click
        end
        object btnZUCEIA32: TButton
          Left = 288
          Top = 72
          Width = 75
          Height = 25
          Caption = 'ZUC EIA3 2'
          TabOrder = 4
          OnClick = btnZUCEIA32Click
        end
        object btnZUCEEA31: TButton
          Left = 168
          Top = 32
          Width = 75
          Height = 25
          Caption = 'ZUC EEA3 1'
          TabOrder = 1
          OnClick = btnZUCEEA31Click
        end
      end
    end
  end
  object OpenDialog1: TOpenDialog
    Left = 220
    Top = 200
  end
  object dlgSave: TSaveDialog
    Left = 404
    Top = 338
  end
end
