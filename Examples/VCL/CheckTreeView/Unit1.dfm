object Form1: TForm1
  Left = 259
  Top = 168
  BorderStyle = bsDialog
  Caption = 'Check TreeView Checked[] and NodeEnabled[] Test'
  ClientHeight = 306
  ClientWidth = 516
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
  object CnCheckTreeView1: TCnCheckTreeView
    Left = 32
    Top = 32
    Width = 273
    Height = 241
    HideSelection = False
    Indent = 19
    ReadOnly = True
    TabOrder = 0
    Items.Data = {
      030000001C000000000000000000000003000000FFFFFFFF0000000002000000
      033131311C000000000000000000000003000000FFFFFFFF0000000002000000
      033131321C000000000000000000000001000000FFFFFFFF0000000000000000
      033131331C000000000000000000000002000000FFFFFFFF0000000000000000
      033131341C000000000000000000000002000000FFFFFFFF0000000000000000
      033132311C000000000000000000000002000000FFFFFFFF0000000001000000
      033132321C000000000000000000000002000000FFFFFFFF0000000000000000
      033132331C000000000000000000000001000000FFFFFFFF0000000000000000
      03313333}
    CanDisableNode = False
  end
  object btnCheck: TButton
    Left = 344
    Top = 32
    Width = 137
    Height = 25
    Caption = 'Check Selected'
    TabOrder = 1
    OnClick = btnCheckClick
  end
  object btnUncheck: TButton
    Left = 344
    Top = 72
    Width = 137
    Height = 25
    Caption = 'Uncheck Selected'
    TabOrder = 2
    OnClick = btnUncheckClick
  end
  object btnEnable: TButton
    Left = 344
    Top = 152
    Width = 137
    Height = 25
    Caption = 'Enable All'
    TabOrder = 3
    OnClick = btnEnableClick
  end
  object btnDisable: TButton
    Left = 344
    Top = 112
    Width = 137
    Height = 25
    Caption = 'Disable Selected'
    TabOrder = 4
    OnClick = btnDisableClick
  end
  object chkCanDisableNode: TCheckBox
    Left = 344
    Top = 256
    Width = 129
    Height = 17
    Caption = 'Can Disable Node'
    Checked = True
    State = cbChecked
    TabOrder = 5
    OnClick = chkCanDisableNodeClick
  end
  object btnHideCheck: TButton
    Left = 344
    Top = 192
    Width = 137
    Height = 25
    Caption = 'Hide Selected Check'
    TabOrder = 6
    OnClick = btnHideCheckClick
  end
end
