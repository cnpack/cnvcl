object Form1: TForm1
  Left = 192
  Top = 107
  BorderStyle = bsDialog
  Caption = '通过键盘钩子屏蔽键盘组合键输入的例子'
  ClientHeight = 283
  ClientWidth = 410
  Color = clBtnFace
  Font.Charset = GB2312_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = '宋体'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 12
  object chkKeyBlocker: TCheckBox
    Left = 24
    Top = 24
    Width = 225
    Height = 17
    Caption = '启动屏蔽功能 (Enable KeyBlocker)'
    TabOrder = 0
    OnClick = chkKeyBlockerClick
  end
  object grpBlocks: TGroupBox
    Left = 24
    Top = 56
    Width = 353
    Height = 193
    Caption = '选择屏蔽以下内容(To Block:)'
    TabOrder = 1
    object Label1: TLabel
      Left = 192
      Top = 152
      Width = 96
      Height = 12
      Caption = 'Nothing Blocked.'
    end
    object chkAltEsc: TCheckBox
      Left = 24
      Top = 24
      Width = 97
      Height = 17
      Caption = 'Alt + Esc'
      TabOrder = 0
      OnClick = chkClick
    end
    object chkAltTab: TCheckBox
      Left = 192
      Top = 24
      Width = 97
      Height = 17
      Caption = 'Alt + Tab'
      TabOrder = 1
      OnClick = chkClick
    end
    object chkCAD: TCheckBox
      Left = 24
      Top = 56
      Width = 129
      Height = 17
      Caption = 'Ctrl + Alt + Del'
      Enabled = False
      TabOrder = 2
      OnClick = chkClick
    end
    object chkCAE: TCheckBox
      Left = 192
      Top = 56
      Width = 137
      Height = 17
      Caption = 'Ctrl + Alt + Enter'
      TabOrder = 3
      OnClick = chkClick
    end
    object chkCtrlEnter: TCheckBox
      Left = 24
      Top = 88
      Width = 97
      Height = 17
      Caption = 'Ctrl + Enter'
      TabOrder = 4
      OnClick = chkClick
    end
    object chkCtrlEsc: TCheckBox
      Left = 192
      Top = 88
      Width = 97
      Height = 17
      Caption = 'Ctrl + Esc'
      TabOrder = 5
      OnClick = chkClick
    end
    object chkPower: TCheckBox
      Left = 24
      Top = 120
      Width = 97
      Height = 17
      Caption = 'Power'
      TabOrder = 6
      OnClick = chkClick
    end
    object chkSleep: TCheckBox
      Left = 192
      Top = 120
      Width = 97
      Height = 17
      Caption = 'Sleep'
      TabOrder = 7
      OnClick = chkClick
    end
    object chkWinApp: TCheckBox
      Left = 24
      Top = 152
      Width = 97
      Height = 17
      Caption = 'Windows'
      TabOrder = 8
      OnClick = chkClick
    end
  end
  object CnKeyBlocker1: TCnKeyBlocker
    BlockAltTab = False
    BlockCtrlEsc = False
    BlockAltEsc = False
    BlockCtrlEnter = False
    BlockSleep = False
    BlockPower = False
    BlockWinApps = False
    BlockCtrlAltEnter = False
    BlockCustomKey = False
    OnBlockKey = CnKeyBlocker1BlockKey
    Left = 264
    Top = 16
  end
end
