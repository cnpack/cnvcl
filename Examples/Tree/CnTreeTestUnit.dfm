object CnTreeTestForm: TCnTreeTestForm
  Left = 192
  Top = 130
  Width = 928
  Height = 480
  Caption = 'CnTree Test'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object tvData: TTreeView
    Left = 24
    Top = 32
    Width = 273
    Height = 385
    Indent = 19
    TabOrder = 0
    Items.Data = {
      030000001C0000000000000000000000FFFFFFFFFFFFFFFF0000000001000000
      033131311C0000000000000000000000FFFFFFFFFFFFFFFF0000000002000000
      033132321C0000000000000000000000FFFFFFFFFFFFFFFF0000000000000000
      033131321C0000000000000000000000FFFFFFFFFFFFFFFF0000000000000000
      033131331C0000000000000000000000FFFFFFFFFFFFFFFF0000000001000000
      033232321C0000000000000000000000FFFFFFFFFFFFFFFF0000000001000000
      033231311C0000000000000000000000FFFFFFFFFFFFFFFF0000000002000000
      033231321D0000000000000000000000FFFFFFFFFFFFFFFF0000000000000000
      04323131311D0000000000000000000000FFFFFFFFFFFFFFFF00000000000000
      0004323131321C0000000000000000000000FFFFFFFFFFFFFFFF000000000100
      0000033333331C0000000000000000000000FFFFFFFFFFFFFFFF000000000000
      000003333131}
  end
  object btnLoadFromTreeView: TButton
    Left = 312
    Top = 32
    Width = 185
    Height = 25
    Caption = 'Load From TreeView'
    TabOrder = 1
    OnClick = btnLoadFromTreeViewClick
  end
  object btnSaveToTreeView: TButton
    Left = 312
    Top = 72
    Width = 185
    Height = 25
    Caption = 'Save To TreeView'
    TabOrder = 2
    OnClick = btnSaveToTreeViewClick
  end
  object btnDepthFirstTraval: TButton
    Left = 312
    Top = 112
    Width = 185
    Height = 25
    Caption = 'Depth First Traval'
    TabOrder = 3
    OnClick = btnDepthFirstTravalClick
  end
  object btnWidthFirstTraval: TButton
    Left = 312
    Top = 152
    Width = 185
    Height = 25
    Caption = 'Width First Traval'
    TabOrder = 4
    OnClick = btnWidthFirstTravalClick
  end
end
