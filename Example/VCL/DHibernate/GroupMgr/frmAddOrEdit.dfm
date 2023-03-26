object FormAddOrEdit: TFormAddOrEdit
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  ClientHeight = 336
  ClientWidth = 406
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lblQQCode: TLabel
    Left = 16
    Top = 19
    Width = 57
    Height = 13
    AutoSize = False
    Caption = 'QQ 号码'
  end
  object lblUserName: TLabel
    Left = 16
    Top = 46
    Width = 57
    Height = 13
    AutoSize = False
    Caption = '用户名'
  end
  object lblSex: TLabel
    Left = 16
    Top = 73
    Width = 57
    Height = 13
    AutoSize = False
    Caption = '性别'
  end
  object lblAge: TLabel
    Left = 16
    Top = 100
    Width = 57
    Height = 13
    AutoSize = False
    Caption = '年龄'
  end
  object lblArea: TLabel
    Left = 16
    Top = 127
    Width = 57
    Height = 13
    AutoSize = False
    Caption = '地区'
  end
  object lblIdCard: TLabel
    Left = 16
    Top = 154
    Width = 57
    Height = 13
    AutoSize = False
    Caption = '群名片'
  end
  object lblEmail: TLabel
    Left = 16
    Top = 181
    Width = 57
    Height = 13
    AutoSize = False
    Caption = '邮箱'
  end
  object lblWebsite: TLabel
    Left = 16
    Top = 208
    Width = 57
    Height = 13
    AutoSize = False
    Caption = '个人主页'
  end
  object lblStat: TLabel
    Left = 16
    Top = 235
    Width = 57
    Height = 13
    AutoSize = False
    Caption = '状态'
  end
  object lblIden: TLabel
    Left = 16
    Top = 262
    Width = 57
    Height = 13
    AutoSize = False
    Caption = '群身份'
  end
  object lblResearch: TLabel
    Left = 208
    Top = 19
    Width = 57
    Height = 13
    AutoSize = False
    Caption = '研究领域'
  end
  object lblOutReason: TLabel
    Left = 208
    Top = 154
    Width = 57
    Height = 13
    AutoSize = False
    Caption = '退群理由'
  end
  object lblInTime: TLabel
    Left = 16
    Top = 289
    Width = 57
    Height = 13
    AutoSize = False
    Caption = '入群时间'
    Visible = False
  end
  object lblOutTime: TLabel
    Left = 208
    Top = 289
    Width = 57
    Height = 13
    AutoSize = False
    Caption = '退群时间'
    Visible = False
  end
  object edtQQCode: TEdit
    Left = 72
    Top = 16
    Width = 121
    Height = 21
    TabOrder = 0
  end
  object edtUserName: TEdit
    Left = 72
    Top = 43
    Width = 121
    Height = 21
    TabOrder = 1
  end
  object edtAge: TEdit
    Left = 72
    Top = 97
    Width = 121
    Height = 21
    TabOrder = 2
  end
  object cbSex: TComboBox
    Left = 72
    Top = 70
    Width = 121
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    ItemIndex = 0
    TabOrder = 3
    Items.Strings = (
      ''
      '男'
      '女')
  end
  object edtArea: TEdit
    Left = 72
    Top = 124
    Width = 121
    Height = 21
    TabOrder = 4
  end
  object edtIdCard: TEdit
    Left = 72
    Top = 151
    Width = 121
    Height = 21
    TabOrder = 5
  end
  object edtEmail: TEdit
    Left = 71
    Top = 178
    Width = 121
    Height = 21
    TabOrder = 6
  end
  object edtWebSite: TEdit
    Left = 72
    Top = 205
    Width = 121
    Height = 21
    TabOrder = 7
  end
  object cbStatus: TComboBox
    Left = 72
    Top = 232
    Width = 121
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    ItemIndex = 0
    TabOrder = 8
    Text = '在群'
    Items.Strings = (
      '在群'
      '已退群')
  end
  object cbIdentity: TComboBox
    Left = 72
    Top = 259
    Width = 121
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    ItemIndex = 0
    TabOrder = 9
    Text = '会员'
    Items.Strings = (
      '会员'
      '管理员')
  end
  object mmResearch: TMemo
    Left = 208
    Top = 43
    Width = 185
    Height = 97
    ScrollBars = ssVertical
    TabOrder = 10
  end
  object mmOutReason: TMemo
    Left = 208
    Top = 178
    Width = 185
    Height = 102
    ScrollBars = ssVertical
    TabOrder = 11
  end
  object gbOperation: TGroupBox
    Left = 0
    Top = 288
    Width = 406
    Height = 48
    Align = alBottom
    TabOrder = 12
    object btnOK: TButton
      Left = 237
      Top = 16
      Width = 75
      Height = 25
      Caption = '确定'
      TabOrder = 0
      OnClick = btnOKClick
    end
    object btnCancel: TButton
      Left = 318
      Top = 16
      Width = 75
      Height = 25
      Caption = '取消'
      TabOrder = 1
      OnClick = btnCancelClick
    end
    object btnViewWarn: TButton
      Left = 16
      Top = 16
      Width = 75
      Height = 25
      Caption = '查看警告'
      TabOrder = 2
      Visible = False
      OnClick = btnViewWarnClick
    end
    object btnViewResearch: TButton
      Left = 97
      Top = 16
      Width = 75
      Height = 25
      Caption = '查看成果'
      TabOrder = 3
      Visible = False
      OnClick = btnViewResearchClick
    end
  end
  object edtInTime: TEdit
    Left = 71
    Top = 286
    Width = 121
    Height = 21
    Enabled = False
    TabOrder = 13
    Visible = False
  end
  object edtOutTime: TEdit
    Left = 272
    Top = 286
    Width = 121
    Height = 21
    Enabled = False
    TabOrder = 14
    Visible = False
  end
end
