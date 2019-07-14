object CnTreeTestForm: TCnTreeTestForm
  Left = 27
  Top = 129
  Width = 928
  Height = 563
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
    Height = 497
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
    Width = 345
    Height = 145
    Caption = 'Common Tree'
    TabOrder = 1
    object btnLoadFromTreeView: TButton
      Left = 16
      Top = 24
      Width = 145
      Height = 25
      Caption = 'Load From TreeView'
      TabOrder = 0
      OnClick = btnLoadFromTreeViewClick
    end
    object btnSaveToTreeView: TButton
      Left = 16
      Top = 64
      Width = 145
      Height = 25
      Caption = 'Save To TreeView'
      TabOrder = 2
      OnClick = btnSaveToTreeViewClick
    end
    object btnDepthFirstTravel: TButton
      Left = 176
      Top = 24
      Width = 145
      Height = 25
      Caption = 'Depth First Travel'
      TabOrder = 1
      OnClick = btnDepthFirstTravelClick
    end
    object btnWidthFirstTravel: TButton
      Left = 176
      Top = 64
      Width = 145
      Height = 25
      Caption = 'Width First Travel'
      TabOrder = 3
      OnClick = btnWidthFirstTravelClick
    end
    object btnTreeHeight: TButton
      Left = 176
      Top = 104
      Width = 145
      Height = 25
      Caption = 'Show Tree Height'
      TabOrder = 4
      OnClick = btnTreeHeightClick
    end
  end
  object grpBTree: TGroupBox
    Left = 312
    Top = 184
    Width = 345
    Height = 225
    Caption = 'Binary Tree'
    TabOrder = 3
    object btnBLoad: TButton
      Left = 16
      Top = 24
      Width = 145
      Height = 25
      Caption = 'Load From TreeView'
      TabOrder = 0
      OnClick = btnBLoadClick
    end
    object btnBSave: TButton
      Left = 16
      Top = 64
      Width = 145
      Height = 25
      Caption = 'Save To TreeView'
      TabOrder = 2
      OnClick = btnBSaveClick
    end
    object btnPreOrderTravel: TButton
      Left = 16
      Top = 104
      Width = 145
      Height = 25
      Caption = 'PreOrder Travel'
      TabOrder = 4
      OnClick = btnPreOrderTravelClick
    end
    object btnInOrderTravel: TButton
      Left = 16
      Top = 144
      Width = 145
      Height = 25
      Caption = 'InOrder Travel'
      TabOrder = 6
      OnClick = btnInOrderTravelClick
    end
    object btnPostOrderTravel: TButton
      Left = 16
      Top = 184
      Width = 145
      Height = 25
      Caption = 'PostOrder Travel'
      TabOrder = 8
      OnClick = btnPostOrderTravelClick
    end
    object btnIsFull: TButton
      Left = 176
      Top = 24
      Width = 145
      Height = 25
      Caption = 'Is Full?'
      TabOrder = 1
      OnClick = btnIsFullClick
    end
    object btnIsComplete: TButton
      Left = 176
      Top = 64
      Width = 145
      Height = 25
      Caption = 'Is Complete?'
      TabOrder = 3
      OnClick = btnIsCompleteClick
    end
    object btnIsBalance: TButton
      Left = 176
      Top = 104
      Width = 145
      Height = 25
      Caption = 'Is Balance?'
      TabOrder = 5
      OnClick = btnIsBalanceClick
    end
    object btnBTreeHeight: TButton
      Left = 176
      Top = 144
      Width = 145
      Height = 25
      Caption = 'Show Tree Height'
      TabOrder = 7
      OnClick = btnBTreeHeightClick
    end
  end
  object grpTrieTree: TGroupBox
    Left = 680
    Top = 24
    Width = 185
    Height = 241
    Caption = 'Trie Tree'
    TabOrder = 2
    object btnSaveTrie: TButton
      Left = 16
      Top = 64
      Width = 145
      Height = 25
      Caption = 'Save To TreeView'
      TabOrder = 1
      OnClick = btnSaveTrieClick
    end
    object btnGenerateTrie: TButton
      Left = 16
      Top = 24
      Width = 145
      Height = 25
      Caption = 'Generate Tree'
      TabOrder = 0
      OnClick = btnGenerateTrieClick
    end
    object btnShowTrieHeight: TButton
      Left = 16
      Top = 144
      Width = 145
      Height = 25
      Caption = 'Show Tree Height'
      TabOrder = 4
      OnClick = btnShowTrieHeightClick
    end
    object btnSearch: TButton
      Left = 96
      Top = 104
      Width = 65
      Height = 25
      Caption = 'Search'
      TabOrder = 2
      OnClick = btnSearchClick
    end
    object edtSearch: TEdit
      Left = 16
      Top = 106
      Width = 73
      Height = 21
      TabOrder = 3
      Text = 'pie'
    end
    object chkAnsi: TCheckBox
      Left = 16
      Top = 184
      Width = 145
      Height = 17
      Caption = 'Ansi Fast Mode'
      TabOrder = 5
    end
    object chkCase: TCheckBox
      Left = 16
      Top = 208
      Width = 145
      Height = 17
      Caption = 'Case Sensitive'
      TabOrder = 6
    end
  end
  object grpBSort: TGroupBox
    Left = 680
    Top = 288
    Width = 185
    Height = 233
    Caption = 'Binary Sort Tree'
    TabOrder = 4
    object btnInit: TButton
      Left = 16
      Top = 24
      Width = 145
      Height = 25
      Caption = 'Init Binary Sort Tree'
      TabOrder = 0
      OnClick = btnInitClick
    end
    object btnBSSearchSelected: TButton
      Left = 16
      Top = 64
      Width = 145
      Height = 25
      Caption = 'Search Selected'
      TabOrder = 1
      OnClick = btnBSSearchSelectedClick
    end
    object btnBSDelete: TButton
      Left = 16
      Top = 104
      Width = 145
      Height = 25
      Caption = 'Delete Selected'
      TabOrder = 2
      OnClick = btnBSDeleteClick
    end
    object btnBSInOrderTravel: TButton
      Left = 16
      Top = 144
      Width = 145
      Height = 25
      Caption = 'InOrder Travel'
      TabOrder = 3
      OnClick = btnBSInOrderTravelClick
    end
    object btnBSPrev: TButton
      Left = 16
      Top = 184
      Width = 73
      Height = 25
      Caption = 'Prev Select'
      TabOrder = 4
      OnClick = btnBSPrevClick
    end
    object btnBSNext: TButton
      Left = 96
      Top = 184
      Width = 67
      Height = 25
      Caption = 'Next Select'
      TabOrder = 5
      OnClick = btnBSNextClick
    end
  end
end
