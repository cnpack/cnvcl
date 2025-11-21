object MLKEMMainForm: TMLKEMMainForm
  Left = 192
  Top = 107
  BorderStyle = bsDialog
  Caption = 'MLKEM Demo'
  ClientHeight = 513
  ClientWidth = 688
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 688
    Height = 233
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 8
      Width = 77
      Height = 13
      Caption = 'MLKEM 类型：'
    end
    object Label2: TLabel
      Left = 328
      Top = 8
      Width = 72
      Height = 13
      Caption = '随机数种子：'
    end
    object Label3: TLabel
      Left = 8
      Top = 32
      Width = 132
      Height = 13
      Caption = '公开密钥（十六进制）：'
    end
    object Label4: TLabel
      Left = 328
      Top = 32
      Width = 144
      Height = 13
      Caption = '非公开密钥（十六进制）：'
    end
    object cbMLKEMType: TComboBox
      Left = 96
      Top = 4
      Width = 121
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 0
    end
    object btnGenerateKeys: TButton
      Left = 232
      Top = 4
      Width = 89
      Height = 25
      Caption = '生成公私钥'
      TabOrder = 1
      OnClick = btnGenerateKeysClick
    end
    object edtRandomSeed: TEdit
      Left = 408
      Top = 4
      Width = 201
      Height = 21
      TabOrder = 2
    end
    object MemoPublicKey: TMemo
      Left = 8
      Top = 56
      Width = 305
      Height = 169
      ScrollBars = ssVertical
      TabOrder = 3
    end
    object MemoPrivateKey: TMemo
      Left = 328
      Top = 56
      Width = 345
      Height = 169
      ScrollBars = ssVertical
      TabOrder = 4
    end
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 233
    Width = 688
    Height = 280
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 1
    object TabSheet1: TTabSheet
      Caption = '加解密'
      object Label5: TLabel
        Left = 8
        Top = 8
        Width = 60
        Height = 13
        Caption = '明文数据：'
      end
      object Label6: TLabel
        Left = 8
        Top = 40
        Width = 60
        Height = 13
        Caption = '加密结果：'
      end
      object Label7: TLabel
        Left = 344
        Top = 40
        Width = 60
        Height = 13
        Caption = '解密结果：'
      end
      object edtPlainMessage: TEdit
        Left = 72
        Top = 4
        Width = 241
        Height = 21
        TabOrder = 0
      end
      object btnEncrypt: TButton
        Left = 320
        Top = 4
        Width = 75
        Height = 25
        Caption = '加密'
        TabOrder = 1
        OnClick = btnEncryptClick
      end
      object btnDecrypt: TButton
        Left = 400
        Top = 4
        Width = 75
        Height = 25
        Caption = '解密'
        TabOrder = 2
        OnClick = btnDecryptClick
      end
      object MemoEncrypted: TMemo
        Left = 8
        Top = 64
        Width = 321
        Height = 185
        ScrollBars = ssVertical
        TabOrder = 3
      end
      object MemoDecrypted: TMemo
        Left = 344
        Top = 64
        Width = 329
        Height = 185
        ScrollBars = ssVertical
        TabOrder = 4
      end
    end
    object TabSheet2: TTabSheet
      Caption = '密钥封装解封'
      ImageIndex = 1
      object Label8: TLabel
        Left = 8
        Top = 8
        Width = 99
        Height = 13
        Caption = '密钥封装共享密钥:'
      end
      object Label9: TLabel
        Left = 8
        Top = 120
        Width = 51
        Height = 13
        Caption = '密文数据:'
      end
      object Label10: TLabel
        Left = 344
        Top = 8
        Width = 99
        Height = 13
        Caption = '密钥解封共享密钥:'
      end
      object btnEncaps: TButton
        Left = 256
        Top = 88
        Width = 75
        Height = 25
        Caption = '密钥封装'
        TabOrder = 0
        OnClick = btnEncapsClick
      end
      object btnDecaps: TButton
        Left = 344
        Top = 88
        Width = 75
        Height = 25
        Caption = '密钥解封'
        TabOrder = 1
        OnClick = btnDecapsClick
      end
      object MemoSharedKey1: TMemo
        Left = 8
        Top = 24
        Width = 321
        Height = 57
        ScrollBars = ssVertical
        TabOrder = 2
      end
      object MemoCipherText: TMemo
        Left = 8
        Top = 144
        Width = 665
        Height = 97
        ScrollBars = ssVertical
        TabOrder = 3
      end
      object MemoSharedKey2: TMemo
        Left = 344
        Top = 24
        Width = 329
        Height = 57
        ScrollBars = ssVertical
        TabOrder = 4
      end
    end
  end
end
