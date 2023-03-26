object FrmCnDialUpDemo: TFrmCnDialUpDemo
  Left = 364
  Top = 247
  BorderStyle = bsDialog
  Caption = 'CnDialUp Demo'
  ClientHeight = 235
  ClientWidth = 455
  Color = clBtnFace
  Font.Charset = GB2312_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = '宋体'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 12
  object lbl1: TLabel
    Left = 28
    Top = 45
    Width = 84
    Height = 12
    Caption = '可用网络连接：'
  end
  object lbl2: TLabel
    Left = 28
    Top = 94
    Width = 48
    Height = 12
    Caption = '用户名：'
  end
  object lbl3: TLabel
    Left = 28
    Top = 144
    Width = 60
    Height = 12
    Caption = '用户密码：'
  end
  object cbbConnections: TComboBox
    Left = 118
    Top = 41
    Width = 299
    Height = 20
    Style = csDropDownList
    ItemHeight = 12
    TabOrder = 0
  end
  object edtUserName: TEdit
    Left = 120
    Top = 85
    Width = 297
    Height = 20
    TabOrder = 1
  end
  object edtPwd: TEdit
    Left = 120
    Top = 136
    Width = 297
    Height = 20
    PasswordChar = '*'
    TabOrder = 2
  end
  object btnConnect: TButton
    Left = 151
    Top = 183
    Width = 75
    Height = 25
    Caption = '开始连接'
    TabOrder = 3
    OnClick = btnConnectClick
  end
  object btnDisConnect: TButton
    Left = 264
    Top = 183
    Width = 75
    Height = 25
    Caption = '断开连接'
    TabOrder = 4
    OnClick = btnDisConnectClick
  end
  object FDialupDemo: TCnDialUp
    PossibleConnections.Strings = (
      '萍乡网通')
    LangStrList.Strings = (
      'Connecting to %s...'
      'Verifying username and password...'
      'An error occured while trying to connect to %s.')
    Left = 32
    Top = 184
  end
end
