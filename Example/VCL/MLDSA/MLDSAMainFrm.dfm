object MLDSAMainForm: TMLDSAMainForm
  Left = 192
  Top = 107
  BorderStyle = bsDialog
  Caption = 'MLDSA Demo'
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
      Width = 76
      Height = 13
      Caption = 'MLDSA 类型：'
    end
    object Label2: TLabel
      Left = 224
      Top = 8
      Width = 72
      Height = 13
      Caption = '随机数种子：'
    end
    object Label3: TLabel
      Left = 8
      Top = 48
      Width = 108
      Height = 13
      Caption = '公钥（十六进制）：'
    end
    object Label4: TLabel
      Left = 328
      Top = 48
      Width = 108
      Height = 13
      Caption = '私钥（十六进制）：'
    end
    object cbMLDSAType: TComboBox
      Left = 96
      Top = 4
      Width = 121
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 0
    end
    object btnGenerateKeys: TButton
      Left = 520
      Top = 4
      Width = 89
      Height = 25
      Caption = '生成公私钥'
      TabOrder = 1
      OnClick = btnGenerateKeysClick
    end
    object edtRandomSeed: TEdit
      Left = 304
      Top = 4
      Width = 201
      Height = 21
      TabOrder = 2
    end
    object MemoPublicKey: TMemo
      Left = 8
      Top = 72
      Width = 305
      Height = 153
      ScrollBars = ssVertical
      TabOrder = 3
    end
    object MemoPrivateKey: TMemo
      Left = 328
      Top = 72
      Width = 345
      Height = 153
      ScrollBars = ssVertical
      TabOrder = 4
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 233
    Width = 688
    Height = 280
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object Label6: TLabel
      Left = 8
      Top = 16
      Width = 60
      Height = 13
      Caption = '明文数据：'
    end
    object Label7: TLabel
      Left = 8
      Top = 144
      Width = 60
      Height = 13
      Caption = '签名结果：'
    end
    object Label5: TLabel
      Left = 464
      Top = 16
      Width = 84
      Height = 13
      Caption = '签名杂凑类型：'
    end
    object edtMessage: TMemo
      Left = 8
      Top = 48
      Width = 665
      Height = 89
      Lines.Strings = (
        'Test Message')
      ScrollBars = ssVertical
      TabOrder = 0
    end
    object btnSign: TButton
      Left = 8
      Top = 240
      Width = 75
      Height = 25
      Caption = '签名'
      TabOrder = 1
      OnClick = btnSignClick
    end
    object edtSignature: TMemo
      Left = 8
      Top = 160
      Width = 665
      Height = 73
      ScrollBars = ssVertical
      TabOrder = 2
    end
    object btnVerify: TButton
      Left = 96
      Top = 240
      Width = 75
      Height = 25
      Caption = '验证签名'
      TabOrder = 3
      OnClick = btnVerifyClick
    end
    object cbHashType: TComboBox
      Left = 552
      Top = 12
      Width = 121
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 4
    end
  end
end
