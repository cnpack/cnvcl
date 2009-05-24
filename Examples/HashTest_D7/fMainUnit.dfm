object Form3: TForm3
  Left = 199
  Top = 146
  Width = 396
  Height = 370
  Caption = 'HashTable Test'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 268
    Top = 38
    Width = 66
    Height = 13
    Anchors = [akTop, akRight]
    Caption = '&Initial Buckets'
    FocusControl = edtInitialBuckets
  end
  object Label2: TLabel
    Left = 268
    Top = 122
    Width = 35
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'Re&peat'
    FocusControl = edtSearchRepeat
  end
  object Label3: TLabel
    Left = 268
    Top = 61
    Width = 63
    Height = 13
    Anchors = [akTop, akRight]
    Caption = '&Rehash Point'
    FocusControl = edtRehashPoint
  end
  object btnHashTableInfo: TSpeedButton
    Left = 251
    Top = 86
    Width = 23
    Height = 23
    Caption = '&?'
    OnClick = btnHashTableInfoClick
  end
  object edtInitialBuckets: TEdit
    Left = 339
    Top = 35
    Width = 41
    Height = 21
    Anchors = [akTop, akRight]
    TabOrder = 2
    Text = '$100'
  end
  object rgHashType: TRadioGroup
    Left = 8
    Top = 35
    Width = 254
    Height = 44
    Anchors = [akLeft, akTop, akRight]
    Caption = ' Hash &Type '
    Columns = 4
    ItemIndex = 3
    Items.Strings = (
      'Small'
      'Medium'
      'Big'
      'Custom')
    TabOrder = 1
    OnClick = rgHashTypeClick
  end
  object btnGenerateHashTable1: TButton
    Tag = 1
    Left = 8
    Top = 85
    Width = 75
    Height = 25
    Caption = '&Sorted'
    TabOrder = 4
    OnClick = btnGenerateHashTable1Click
  end
  object mmoLog: TMemo
    Left = 8
    Top = 179
    Width = 372
    Height = 156
    Anchors = [akLeft, akTop, akRight, akBottom]
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 14
  end
  object btnGenerateHashTable2: TButton
    Tag = 2
    Left = 89
    Top = 85
    Width = 75
    Height = 25
    Caption = '&No sort (slow)'
    TabOrder = 5
    OnClick = btnGenerateHashTable1Click
  end
  object btnGenerateHashTable3: TButton
    Tag = 3
    Left = 170
    Top = 85
    Width = 75
    Height = 25
    Caption = '&Copy-sort'
    TabOrder = 6
    OnClick = btnGenerateHashTable1Click
  end
  object btnCleanLog: TButton
    Left = 305
    Top = 85
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Clear &Log'
    TabOrder = 7
    OnClick = btnCleanLogClick
  end
  object edtSourceFile: TEdit
    Left = 8
    Top = 8
    Width = 372
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    Text = 'test.txt'
  end
  object btnSearch: TButton
    Left = 8
    Top = 117
    Width = 75
    Height = 25
    Caption = 'S&earch'
    TabOrder = 8
    OnClick = btnSearchClick
  end
  object edtSearch: TEdit
    Left = 89
    Top = 119
    Width = 173
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 9
    Text = 'Search Text'
  end
  object edtSearchRepeat: TEdit
    Left = 305
    Top = 119
    Width = 75
    Height = 21
    Anchors = [akTop, akRight]
    TabOrder = 10
    Text = '1000000'
  end
  object btnTravel: TButton
    Left = 8
    Top = 148
    Width = 75
    Height = 25
    Caption = 'Tr&avel'
    TabOrder = 11
    OnClick = btnTravelClick
  end
  object edtTravelFile: TEdit
    Left = 170
    Top = 150
    Width = 210
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 13
    Text = 'test.out.txt'
  end
  object edtRehashPoint: TEdit
    Left = 339
    Top = 58
    Width = 41
    Height = 21
    Anchors = [akTop, akRight]
    TabOrder = 3
    Text = '$80'
  end
  object btnTravelSorted: TButton
    Left = 89
    Top = 148
    Width = 75
    Height = 25
    Caption = 'Sorted tra&vel'
    TabOrder = 12
    OnClick = btnTravelSortedClick
  end
end
