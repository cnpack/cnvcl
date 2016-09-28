object FormCrypt: TFormCrypt
  Left = 320
  Top = 138
  BorderStyle = bsDialog
  Caption = 'Crypt/Decrypt DEMO'
  ClientHeight = 414
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
    Height = 361
    ActivePage = tsDES
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
        object edtFrom: TEdit
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
          TabOrder = 1
          OnClick = btnMd5Click
        end
        object pnlMd5: TPanel
          Left = 24
          Top = 136
          Width = 233
          Height = 41
          BevelOuter = bvNone
          TabOrder = 2
        end
        object btnMd5File: TButton
          Left = 72
          Top = 208
          Width = 75
          Height = 25
          Caption = 'File MD5'
          TabOrder = 3
          OnClick = btnMd5FileClick
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
        object edt3: TEdit
          Left = 72
          Top = 140
          Width = 169
          Height = 21
          TabOrder = 2
        end
        object Button2: TButton
          Left = 72
          Top = 190
          Width = 105
          Height = 25
          Caption = 'Base64 Decode'
          TabOrder = 3
          OnClick = Button2Click
        end
        object edt4: TEdit
          Left = 72
          Top = 244
          Width = 169
          Height = 21
          TabOrder = 4
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
          TabOrder = 1
          OnClick = btnCRC32Click
        end
        object pnlCRC32: TPanel
          Left = 24
          Top = 144
          Width = 233
          Height = 41
          BevelOuter = bvNone
          TabOrder = 2
          OnDblClick = ResultDblClick
        end
        object btnFileCRC32: TButton
          Left = 72
          Top = 208
          Width = 75
          Height = 25
          Caption = 'File CRC32'
          TabOrder = 3
          OnClick = btnFileCRC32Click
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
          TabOrder = 1
          OnClick = btnCRC64Click
        end
        object pnlCRC64: TPanel
          Left = 24
          Top = 144
          Width = 233
          Height = 41
          BevelOuter = bvNone
          TabOrder = 2
          OnDblClick = ResultDblClick
        end
        object btnFileCRC64: TButton
          Left = 72
          Top = 208
          Width = 75
          Height = 25
          Caption = 'File CRC64'
          TabOrder = 3
          OnClick = btnFileCRC64Click
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
          TabOrder = 1
          OnClick = btnSha1Click
        end
        object pnlSha1: TPanel
          Left = 8
          Top = 136
          Width = 281
          Height = 41
          BevelOuter = bvNone
          TabOrder = 2
          OnDblClick = ResultDblClick
        end
        object btnFileSha1: TButton
          Left = 72
          Top = 208
          Width = 75
          Height = 25
          Caption = 'File SHA1'
          TabOrder = 3
          OnClick = btnFileSha1Click
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
          TabOrder = 1
          OnClick = btnSM3Click
        end
        object btnFileSM3: TButton
          Left = 72
          Top = 208
          Width = 75
          Height = 25
          Caption = 'File SM3'
          TabOrder = 2
          OnClick = btnFileSM3Click
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
          TabOrder = 5
        end
        object btnSm4Dec: TButton
          Left = 72
          Top = 200
          Width = 75
          Height = 25
          Caption = 'SM4 Decrypt'
          TabOrder = 4
          OnClick = btnSm4DecClick
        end
        object edtSm4Code: TEdit
          Left = 72
          Top = 160
          Width = 169
          Height = 21
          TabOrder = 3
        end
        object rbSm4Ecb: TRadioButton
          Left = 168
          Top = 116
          Width = 57
          Height = 17
          Caption = 'ECB'
          TabOrder = 6
        end
        object rbSm4CBC: TRadioButton
          Left = 232
          Top = 116
          Width = 57
          Height = 17
          Caption = 'CBC'
          Checked = True
          TabOrder = 7
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
          TabOrder = 2
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
          TabOrder = 5
        end
        object btnAesDecrypt: TButton
          Left = 72
          Top = 208
          Width = 75
          Height = 25
          Caption = 'AES Decrypt'
          TabOrder = 4
          OnClick = btnAesDecryptClick
        end
        object edtAesResult: TEdit
          Left = 72
          Top = 176
          Width = 169
          Height = 21
          TabOrder = 3
        end
        object rbAesecb: TRadioButton
          Left = 168
          Top = 132
          Width = 57
          Height = 17
          Caption = 'ECB'
          TabOrder = 6
        end
        object rbAescbc: TRadioButton
          Left = 232
          Top = 132
          Width = 57
          Height = 17
          Caption = 'CBC'
          Checked = True
          TabOrder = 7
          TabStop = True
        end
        object cbbAesKeyBitType: TComboBox
          Left = 72
          Top = 96
          Width = 169
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 8
          Items.Strings = (
            '128 Bit'
            '192 Bit'
            '256 Bit')
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
          TabOrder = 1
          OnClick = btnSHA256Click
        end
        object pnlSHA256: TPanel
          Left = 8
          Top = 136
          Width = 401
          Height = 41
          BevelOuter = bvNone
          TabOrder = 2
          OnDblClick = ResultDblClick
        end
        object btnFileSHA256: TButton
          Left = 72
          Top = 208
          Width = 75
          Height = 25
          Caption = 'File SHA256'
          TabOrder = 3
          OnClick = btnFileSHA256Click
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
          TabOrder = 1
          OnClick = btnSHA224Click
        end
        object pnlSHA224: TPanel
          Left = 8
          Top = 136
          Width = 401
          Height = 41
          BevelOuter = bvNone
          TabOrder = 2
          OnDblClick = ResultDblClick
        end
        object btnSHA224File: TButton
          Left = 72
          Top = 208
          Width = 75
          Height = 25
          Caption = 'File SHA224'
          TabOrder = 3
          OnClick = btnSHA224FileClick
        end
      end
    end
  end
  object OpenDialog1: TOpenDialog
    Left = 220
    Top = 200
  end
end
