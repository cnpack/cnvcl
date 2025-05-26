object FormFloat: TFormFloat
  Left = 499
  Top = 237
  Width = 465
  Height = 361
  Caption = '浮点数转换成字符串 & Extract Float 测试'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 60
    Height = 13
    Caption = '输入浮点数'
  end
  object bvl1: TBevel
    Left = 16
    Top = 140
    Width = 417
    Height = 18
    Shape = bsBottomLine
  end
  object lblExp: TLabel
    Left = 96
    Top = 284
    Width = 36
    Height = 13
    Caption = '指数：'
  end
  object lblManti: TLabel
    Left = 232
    Top = 284
    Width = 60
    Height = 13
    Caption = '有效数字：'
  end
  object Edit1: TEdit
    Left = 8
    Top = 27
    Width = 337
    Height = 21
    TabOrder = 0
    Text = '128.125'
  end
  object CheckBox1: TCheckBox
    Left = 8
    Top = 54
    Width = 121
    Height = 17
    Caption = '十进制指数部分'
    TabOrder = 1
  end
  object CheckBox2: TCheckBox
    Left = 8
    Top = 77
    Width = 97
    Height = 17
    Caption = '一定使用指数'
    TabOrder = 2
  end
  object Button1: TButton
    Left = 353
    Top = 25
    Width = 75
    Height = 25
    Caption = '转换'
    TabOrder = 3
    OnClick = Button1Click
  end
  object rdoBin: TRadioButton
    Left = 8
    Top = 112
    Width = 113
    Height = 17
    Caption = '二进制(Binary)'
    Checked = True
    TabOrder = 4
    TabStop = True
  end
  object rdoOct: TRadioButton
    Left = 127
    Top = 112
    Width = 113
    Height = 17
    Caption = '八进制(Octal)'
    TabOrder = 5
  end
  object rdoHex: TRadioButton
    Left = 246
    Top = 112
    Width = 139
    Height = 17
    Caption = '十六进制(Hexdecimal)'
    TabOrder = 6
  end
  object btnExtract: TButton
    Left = 352
    Top = 64
    Width = 75
    Height = 25
    Caption = 'Extract'
    TabOrder = 7
    OnClick = btnExtractClick
  end
  object btnUInt64ToFloat: TButton
    Left = 144
    Top = 64
    Width = 97
    Height = 25
    Caption = 'UInt64 To Float'
    TabOrder = 8
    OnClick = btnUInt64ToFloatClick
  end
  object btnFloatToUInt64: TButton
    Left = 248
    Top = 64
    Width = 97
    Height = 25
    Caption = 'Float To UInt64'
    TabOrder = 9
    OnClick = btnFloatToUInt64Click
  end
  object edtFloat: TEdit
    Left = 144
    Top = 184
    Width = 289
    Height = 21
    TabOrder = 10
    Text = '3.14'
    OnChange = edtFloatChange
  end
  object rgFloat: TRadioGroup
    Left = 16
    Top = 180
    Width = 113
    Height = 89
    Caption = '浮点类型：'
    ItemIndex = 2
    Items.Strings = (
      'Single'
      'Double'
      'Extended')
    TabOrder = 11
    OnClick = edtFloatChange
  end
  object edtFloatHex: TEdit
    Left = 144
    Top = 216
    Width = 289
    Height = 21
    TabOrder = 12
  end
  object edtFloatBack: TEdit
    Left = 144
    Top = 248
    Width = 289
    Height = 21
    TabOrder = 13
  end
  object chkNeg: TCheckBox
    Left = 16
    Top = 282
    Width = 57
    Height = 17
    Caption = '负滴'
    TabOrder = 14
  end
  object edtExp: TEdit
    Left = 136
    Top = 280
    Width = 73
    Height = 21
    TabOrder = 15
  end
  object edtManti: TEdit
    Left = 288
    Top = 280
    Width = 145
    Height = 21
    TabOrder = 16
  end
end
