object FrmTransFilter: TFrmTransFilter
  Left = 332
  Top = 271
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = '过滤设置'
  ClientHeight = 240
  ClientWidth = 192
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
  object lblFilter: TLabel
    Left = 8
    Top = 8
    Width = 168
    Height = 13
    Caption = '只为以下属性生成翻译字符串：'
  end
  object chklstFilter: TCheckListBox
    Left = 8
    Top = 26
    Width = 177
    Height = 175
    ItemHeight = 13
    Items.Strings = (
      'Font'
      'Caption'
      'Category'
      'HelpKeyword'
      'Hint'
      'Text'
      'ImeName'
      'Title'
      'DefaultExt'
      'Filter'
      'InitialDir'
      'SubItems[].Text'
      '*Others*')
    TabOrder = 0
    OnKeyPress = chklstFilterKeyPress
  end
  object btnCancel: TButton
    Left = 110
    Top = 210
    Width = 75
    Height = 21
    Cancel = True
    Caption = '取消(&C)'
    ModalResult = 2
    TabOrder = 2
  end
  object btnOK: TButton
    Left = 30
    Top = 210
    Width = 75
    Height = 21
    Caption = '确定(&O)'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
end
