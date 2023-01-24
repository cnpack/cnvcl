object FormAddWarning: TFormAddWarning
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = '添加警告'
  ClientHeight = 266
  ClientWidth = 271
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object lblQQCode: TLabel
    Left = 16
    Top = 16
    Width = 49
    Height = 13
    AutoSize = False
    Caption = 'QQ 号码'
  end
  object lblUserName: TLabel
    Left = 16
    Top = 43
    Width = 49
    Height = 13
    AutoSize = False
    Caption = '昵称'
  end
  object lblNameCard: TLabel
    Left = 16
    Top = 70
    Width = 49
    Height = 13
    AutoSize = False
    Caption = '群名片'
  end
  object lblReason: TLabel
    Left = 16
    Top = 97
    Width = 49
    Height = 13
    AutoSize = False
    Caption = '警告理由'
  end
  object edtQQCode: TEdit
    Left = 69
    Top = 13
    Width = 185
    Height = 21
    Color = clSilver
    ReadOnly = True
    TabOrder = 0
  end
  object edtUserName: TEdit
    Left = 69
    Top = 40
    Width = 185
    Height = 21
    Color = clSilver
    ReadOnly = True
    TabOrder = 1
  end
  object edtNameCard: TEdit
    Left = 69
    Top = 67
    Width = 185
    Height = 21
    Color = clSilver
    ReadOnly = True
    TabOrder = 2
  end
  object mmReason: TMemo
    Left = 69
    Top = 94
    Width = 185
    Height = 115
    ScrollBars = ssVertical
    TabOrder = 3
  end
  object gbOperation: TGroupBox
    Left = 0
    Top = 221
    Width = 271
    Height = 45
    Align = alBottom
    TabOrder = 4
    ExplicitTop = 240
    object btnOK: TButton
      Left = 98
      Top = 11
      Width = 75
      Height = 25
      Caption = '确定'
      TabOrder = 0
      OnClick = btnOKClick
    end
    object btnCancel: TButton
      Left = 179
      Top = 11
      Width = 75
      Height = 25
      Caption = '取消'
      TabOrder = 1
      OnClick = btnCancelClick
    end
  end
end
