object CnTreeTestForm: TCnTreeTestForm
  Left = 27
  Top = 129
  Width = 928
  Height = 500
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
    Left = 16
    Top = 24
    Width = 273
    Height = 433
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
  object grpTree: TGroupBox
    Left = 312
    Top = 24
    Width = 225
    Height = 193
    Caption = 'Common Tree'
    TabOrder = 1
    object btnLoadFromTreeView: TButton
      Left = 16
      Top = 24
      Width = 185
      Height = 25
      Caption = 'Load From TreeView'
      TabOrder = 0
      OnClick = btnLoadFromTreeViewClick
    end
    object btnSaveToTreeView: TButton
      Left = 16
      Top = 64
      Width = 185
      Height = 25
      Caption = 'Save To TreeView'
      TabOrder = 1
      OnClick = btnSaveToTreeViewClick
    end
    object btnDepthFirstTravel: TButton
      Left = 16
      Top = 104
      Width = 185
      Height = 25
      Caption = 'Depth First Travel'
      TabOrder = 2
      OnClick = btnDepthFirstTravelClick
    end
    object btnWidthFirstTravel: TButton
      Left = 16
      Top = 144
      Width = 185
      Height = 25
      Caption = 'Width First Travel'
      TabOrder = 3
      OnClick = btnWidthFirstTravelClick
    end
  end
  object grpBTree: TGroupBox
    Left = 312
    Top = 232
    Width = 225
    Height = 225
    Caption = 'Binary Tree'
    TabOrder = 2
    object btnBLoad: TButton
      Left = 16
      Top = 24
      Width = 185
      Height = 25
      Caption = 'Load From TreeView'
      TabOrder = 0
      OnClick = btnBLoadClick
    end
    object btnBSave: TButton
      Left = 16
      Top = 64
      Width = 185
      Height = 25
      Caption = 'Save To TreeView'
      TabOrder = 1
      OnClick = btnBSaveClick
    end
    object btnPreOrderTravel: TButton
      Left = 16
      Top = 104
      Width = 185
      Height = 25
      Caption = 'PreOrder Travel'
      TabOrder = 2
      OnClick = btnPreOrderTravelClick
    end
    object btnInOrderTravel: TButton
      Left = 16
      Top = 144
      Width = 185
      Height = 25
      Caption = 'InOrder Travel'
      TabOrder = 3
      OnClick = btnInOrderTravelClick
    end
    object btnPostOrderTravel: TButton
      Left = 16
      Top = 184
      Width = 185
      Height = 25
      Caption = 'PostOrder Travel'
      TabOrder = 4
      OnClick = btnPostOrderTravelClick
    end
  end
end
