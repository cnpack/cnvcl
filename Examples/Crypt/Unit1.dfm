object Form1: TForm1
  Left = 192
  Top = 107
  BorderStyle = bsDialog
  Caption = 'Crypt/Decrypt DEMO'
  ClientHeight = 414
  ClientWidth = 402
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
    Width = 353
    Height = 361
    ActivePage = ts1
    TabOrder = 0
    object ts1: TTabSheet
      Caption = 'DES'
      object grpdES: TGroupBox
        Left = 24
        Top = 24
        Width = 305
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
        object edt1: TEdit
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
    object ts2: TTabSheet
      Caption = 'MD5'
      ImageIndex = 1
      object grpMd5: TGroupBox
        Left = 24
        Top = 24
        Width = 297
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
    object ts3: TTabSheet
      Caption = 'Base64'
      ImageIndex = 2
      object GroupBox1: TGroupBox
        Left = 24
        Top = 24
        Width = 297
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
        Width = 297
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
    object ts64: TTabSheet
      Caption = 'CRC64'
      ImageIndex = 4
      object grp1: TGroupBox
        Left = 24
        Top = 24
        Width = 297
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
        Width = 297
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
  end
  object OpenDialog1: TOpenDialog
    Left = 220
    Top = 200
  end
end
