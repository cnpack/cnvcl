object Form1: TForm1
  Left = 223
  Top = 88
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Form1'
  ClientHeight = 602
  ClientWidth = 604
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 15
  object GroupBox1: TGroupBox
    Left = 19
    Top = 14
    Width = 566
    Height = 232
    Caption = '测试'
    TabOrder = 0
    object Label1: TLabel
      Left = 321
      Top = 58
      Width = 63
      Height = 15
      Caption = '请输入整数:'
    end
    object Label4: TLabel
      Left = 321
      Top = 125
      Width = 150
      Height = 13
      AutoSize = False
      Caption = '请输入字符:'
    end
    object edit1: TEdit
      Left = 320
      Top = 85
      Width = 209
      Height = 23
      TabOrder = 0
      OnChange = Edit1Change
    end
    object Edit2: TEdit
      Left = 320
      Top = 143
      Width = 209
      Height = 23
      TabOrder = 1
      OnChange = Edit2Change
    end
    object Panel1: TPanel
      Left = 11
      Top = 20
      Width = 282
      Height = 202
      BevelInner = bvRaised
      BevelOuter = bvLowered
      TabOrder = 2
      object RadioGroup1: TRadioGroup
        Left = 6
        Top = 3
        Width = 99
        Height = 107
        Caption = '显示样式'
        ItemIndex = 1
        Items.Strings = (
          '闪烁'
          '间歇性闪烁'
          '不闪烁')
        TabOrder = 0
      end
      object ComboBox1: TComboBox
        Left = 117
        Top = 8
        Width = 135
        Height = 23
        Style = csDropDownList
        ItemHeight = 15
        TabOrder = 1
        Items.Strings = (
          'icon1'
          'icon2'
          'icon3'
          'icon4'
          'icon5'
          'icon6'
          'icon7')
      end
      object Button3: TButton
        Left = 117
        Top = 84
        Width = 135
        Height = 25
        Caption = '测试'
        TabOrder = 2
        OnClick = Button3Click
      end
      object ed_tip: TEdit
        Left = 117
        Top = 58
        Width = 135
        Height = 23
        TabOrder = 3
        Text = '提示信息'
      end
      object ComboBox2: TComboBox
        Left = 117
        Top = 33
        Width = 135
        Height = 23
        Style = csDropDownList
        ItemHeight = 15
        TabOrder = 4
        Items.Strings = (
          '左上'
          '右上'
          '左中'
          '右中'
          '左下'
          '右下'
          '上中'
          '下中'
          '上左对齐'
          '下左对齐'
          '上右对齐'
          '下右对齐')
      end
      object ed_test: TMemo
        Left = 66
        Top = 131
        Width = 150
        Height = 48
        Lines.Strings = (
          '测试')
        TabOrder = 5
      end
    end
  end
  object GroupBox2: TGroupBox
    Left = 19
    Top = 252
    Width = 566
    Height = 336
    Caption = '表单提交演示'
    TabOrder = 1
    object Label2: TLabel
      Left = 64
      Top = 27
      Width = 41
      Height = 15
      Caption = '用户名*'
    end
    object Label3: TLabel
      Left = 76
      Top = 60
      Width = 29
      Height = 15
      Caption = '密码*'
    end
    object Label5: TLabel
      Left = 52
      Top = 93
      Width = 53
      Height = 15
      Caption = '重复密码*'
    end
    object Label6: TLabel
      Left = 71
      Top = 127
      Width = 34
      Height = 15
      Caption = 'E-Mail'
    end
    object Label7: TLabel
      Left = 81
      Top = 162
      Width = 24
      Height = 15
      Caption = '年龄'
    end
    object Label8: TLabel
      Left = 81
      Top = 191
      Width = 24
      Height = 15
      Caption = '签名'
    end
    object Label9: TLabel
      Left = 57
      Top = 266
      Width = 48
      Height = 15
      Caption = '上传照片'
    end
    object ed_name: TEdit
      Left = 112
      Top = 24
      Width = 193
      Height = 23
      TabOrder = 0
    end
    object ed_pw: TEdit
      Left = 112
      Top = 57
      Width = 193
      Height = 23
      PasswordChar = '*'
      TabOrder = 1
    end
    object ed_rpw: TEdit
      Left = 112
      Top = 91
      Width = 193
      Height = 23
      PasswordChar = '*'
      TabOrder = 2
    end
    object ed_email: TEdit
      Left = 112
      Top = 124
      Width = 193
      Height = 23
      TabOrder = 3
    end
    object Memo1: TMemo
      Left = 112
      Top = 191
      Width = 353
      Height = 36
      TabOrder = 5
    end
    object ed_age: TEdit
      Left = 112
      Top = 158
      Width = 193
      Height = 23
      TabOrder = 4
    end
    object Button1: TButton
      Left = 231
      Top = 298
      Width = 75
      Height = 25
      Caption = '提交'
      TabOrder = 8
      OnClick = Button1Click
    end
    object ed_img: TEdit
      Left = 112
      Top = 264
      Width = 193
      Height = 23
      TabOrder = 6
    end
    object Button2: TButton
      Left = 308
      Top = 264
      Width = 31
      Height = 21
      Caption = '...'
      TabOrder = 7
      OnClick = Button2Click
    end
  end
  object er_test: TCnErrorProvider
    DoubleBuffer = True
    Left = 506
    Top = 6
  end
  object OpenPictureDialog1: TOpenPictureDialog
    Left = 538
    Top = 6
  end
  object er_demo: TCnErrorProvider
    DoubleBuffer = False
    Left = 570
    Top = 6
  end
end
