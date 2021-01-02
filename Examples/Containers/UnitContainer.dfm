object FormContainers: TFormContainers
  Left = 260
  Top = 121
  BorderStyle = bsDialog
  Caption = 'Test Containers'
  ClientHeight = 519
  ClientWidth = 984
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
  object lblCount: TLabel
    Left = 744
    Top = 112
    Width = 34
    Height = 13
    Caption = 'Count: '
  end
  object lblRingBuffer: TLabel
    Left = 40
    Top = 16
    Width = 56
    Height = 13
    Caption = 'Ring Buffer:'
  end
  object bvl1: TBevel
    Left = 32
    Top = 152
    Width = 737
    Height = 17
    Shape = bsTopLine
  end
  object bvl2: TBevel
    Left = 808
    Top = 8
    Width = 17
    Height = 145
    Shape = bsLeftLine
  end
  object lblHashMapCount: TLabel
    Left = 832
    Top = 496
    Width = 31
    Height = 13
    Caption = 'Count:'
  end
  object lblHashMapCapacity: TLabel
    Left = 600
    Top = 496
    Width = 44
    Height = 13
    Caption = 'Capacity:'
  end
  object StringGrid: TStringGrid
    Left = 40
    Top = 40
    Width = 737
    Height = 41
    FixedCols = 0
    RowCount = 1
    FixedRows = 0
    TabOrder = 0
  end
  object btnPushFront: TButton
    Left = 40
    Top = 112
    Width = 97
    Height = 25
    Caption = 'Push to Front'
    TabOrder = 1
    OnClick = btnPushFrontClick
  end
  object btnPushBack: TButton
    Left = 344
    Top = 112
    Width = 91
    Height = 25
    Caption = 'Push to Back'
    TabOrder = 2
    OnClick = btnPushBackClick
  end
  object btnPopFront: TButton
    Left = 448
    Top = 112
    Width = 89
    Height = 25
    Caption = 'Pop from Front'
    TabOrder = 3
    OnClick = btnPopFrontClick
  end
  object btnPopBack: TButton
    Left = 152
    Top = 112
    Width = 97
    Height = 25
    Caption = 'Pop from Back'
    TabOrder = 4
    OnClick = btnPopBackClick
  end
  object sgMap: TStringGrid
    Left = 32
    Top = 176
    Width = 913
    Height = 313
    ColCount = 32
    DefaultColWidth = 24
    FixedCols = 0
    TabOrder = 5
  end
  object btnHashMapAdd: TButton
    Left = 840
    Top = 16
    Width = 105
    Height = 25
    Caption = 'HashMap Add Int'
    TabOrder = 6
    OnClick = btnHashMapAddClick
  end
  object btnHashMapDel: TButton
    Left = 840
    Top = 56
    Width = 105
    Height = 25
    Caption = 'HashMap Del Int'
    TabOrder = 7
    OnClick = btnHashMapDelClick
  end
  object btnHashMapClear: TButton
    Left = 840
    Top = 136
    Width = 105
    Height = 25
    Caption = 'HashMap Clear'
    TabOrder = 8
    OnClick = btnHashMapClearClick
  end
  object btnHashMapAddInt64: TButton
    Left = 840
    Top = 96
    Width = 105
    Height = 25
    Caption = 'HashMap Add Int64'
    TabOrder = 9
    OnClick = btnHashMapAddInt64Click
  end
end
