object SLHDSAMainForm: TSLHDSAMainForm
  Left = 192
  Top = 107
  BorderStyle = bsDialog
  Caption = 'SLH-DSA Demo'
  ClientHeight = 513
  ClientWidth = 688
  Color = clBtnFace
  Font.Charset = GB2312_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = '宋体'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 12
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
      Width = 96
      Height = 12
      Caption = 'SLH-DSA 参数集：'
    end
    object Label3: TLabel
      Left = 8
      Top = 48
      Width = 108
      Height = 12
      Caption = '公钥（十六进制）：'
    end
    object Label4: TLabel
      Left = 336
      Top = 48
      Width = 108
      Height = 12
      Caption = '私钥（十六进制）：'
    end
    object lblKeyInfo: TLabel
      Left = 232
      Top = 8
      Width = 225
      Height = 13
      Caption = '密钥信息'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object cbParamSet: TComboBox
      Left = 100
      Top = 4
      Width = 125
      Height = 20
      Style = csDropDownList
      ItemHeight = 12
      TabOrder = 0
    end
    object btnGenerateKeys: TButton
      Left = 584
      Top = 4
      Width = 89
      Height = 25
      Caption = '生成公私钥'
      TabOrder = 1
      OnClick = btnGenerateKeysClick
    end
    object chkRandomize: TCheckBox
      Left = 472
      Top = 6
      Width = 105
      Height = 17
      Caption = '随机化签名'
      Checked = True
      State = cbChecked
      TabOrder = 2
    end
    object MemoPublicKey: TMemo
      Left = 8
      Top = 72
      Width = 313
      Height = 153
      ScrollBars = ssVertical
      TabOrder = 3
    end
    object MemoPrivateKey: TMemo
      Left = 336
      Top = 72
      Width = 337
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
    object Label2: TLabel
      Left = 8
      Top = 8
      Width = 60
      Height = 12
      Caption = '签名消息：'
    end
    object Label5: TLabel
      Left = 440
      Top = 8
      Width = 72
      Height = 12
      Caption = '预杂凑类型：'
    end
    object Label6: TLabel
      Left = 8
      Top = 144
      Width = 60
      Height = 12
      Caption = '签名结果：'
    end
    object edtMessage: TMemo
      Left = 8
      Top = 24
      Width = 417
      Height = 105
      Lines.Strings = (
        'Test Message')
      ScrollBars = ssVertical
      TabOrder = 0
    end
    object btnSign: TButton
      Left = 8
      Top = 244
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
      Top = 244
      Width = 75
      Height = 25
      Caption = '验证签名'
      TabOrder = 3
      OnClick = btnVerifyClick
    end
    object cbPrehashType: TComboBox
      Left = 528
      Top = 4
      Width = 129
      Height = 20
      Style = csDropDownList
      ItemHeight = 12
      TabOrder = 4
    end
    object chkPrehash: TCheckBox
      Left = 440
      Top = 32
      Width = 97
      Height = 17
      Caption = '预杂凑模式'
      TabOrder = 5
      OnClick = chkPrehashClick
    end
  end
end
