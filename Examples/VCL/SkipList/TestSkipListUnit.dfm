object SkipListTestForm: TSkipListTestForm
  Left = 192
  Top = 130
  Width = 928
  Height = 480
  Caption = 'Skip List Test'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object bvl1: TBevel
    Left = 232
    Top = 16
    Width = 9
    Height = 25
    Shape = bsLeftLine
  end
  object bvl2: TBevel
    Left = 384
    Top = 16
    Width = 9
    Height = 25
    Shape = bsLeftLine
  end
  object bvl3: TBevel
    Left = 488
    Top = 16
    Width = 9
    Height = 25
    Shape = bsLeftLine
  end
  object bvl4: TBevel
    Left = 640
    Top = 16
    Width = 9
    Height = 25
    Shape = bsLeftLine
  end
  object Grid: TStringGrid
    Left = 16
    Top = 64
    Width = 881
    Height = 353
    DefaultColWidth = 24
    FixedRows = 0
    TabOrder = 10
  end
  object btnShow: TButton
    Left = 784
    Top = 16
    Width = 113
    Height = 25
    Caption = 'Show SkipList'
    TabOrder = 7
    OnClick = btnShowClick
  end
  object btnAdd1: TButton
    Left = 16
    Top = 16
    Width = 57
    Height = 25
    Caption = 'Add 1'
    TabOrder = 0
    OnClick = btnAdd1Click
  end
  object btnAdd2: TButton
    Left = 88
    Top = 16
    Width = 57
    Height = 25
    Caption = 'Add 2'
    TabOrder = 1
    OnClick = btnAdd2Click
  end
  object btnAdd3: TButton
    Left = 160
    Top = 16
    Width = 57
    Height = 25
    Caption = 'Add 3'
    TabOrder = 2
    OnClick = btnAdd3Click
  end
  object edtValue: TEdit
    Left = 248
    Top = 18
    Width = 41
    Height = 21
    TabOrder = 8
    Text = '10'
  end
  object udValue: TUpDown
    Left = 289
    Top = 18
    Width = 16
    Height = 21
    Associate = edtValue
    Min = -100
    Position = 10
    TabOrder = 9
    Wrap = False
  end
  object btnAdd: TButton
    Left = 312
    Top = 16
    Width = 57
    Height = 25
    Caption = 'Add'
    TabOrder = 3
    OnClick = btnAddClick
  end
  object btnRandom: TButton
    Left = 400
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Random Add'
    TabOrder = 4
    OnClick = btnRandomClick
  end
  object btnDel: TButton
    Left = 504
    Top = 16
    Width = 121
    Height = 25
    Caption = 'Delete Select Column'
    TabOrder = 5
    OnClick = btnDelClick
  end
  object btnSearch: TButton
    Left = 656
    Top = 16
    Width = 73
    Height = 25
    Caption = 'Search'
    TabOrder = 6
    OnClick = btnSearchClick
  end
end
