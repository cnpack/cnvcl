object FormMatrix: TFormMatrix
  Left = 199
  Top = 156
  Width = 979
  Height = 563
  Caption = 'Matrix Test'
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
  object btnEqual: TSpeedButton
    Left = 648
    Top = 96
    Width = 23
    Height = 22
    Caption = '='
    Flat = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -24
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    OnClick = btnEqualClick
  end
  object StringGrid1: TStringGrid
    Left = 24
    Top = 32
    Width = 345
    Height = 169
    FixedCols = 0
    RowCount = 6
    FixedRows = 0
    TabOrder = 0
  end
  object StaticText1: TStaticText
    Left = 392
    Top = 104
    Width = 21
    Height = 33
    Caption = 'X'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -24
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    OnClick = StaticText1Click
  end
  object StringGrid2: TStringGrid
    Left = 424
    Top = 32
    Width = 209
    Height = 169
    ColCount = 3
    FixedCols = 0
    FixedRows = 0
    TabOrder = 2
  end
  object StringGridR: TStringGrid
    Left = 688
    Top = 32
    Width = 241
    Height = 169
    ColCount = 1
    FixedCols = 0
    RowCount = 1
    FixedRows = 0
    TabOrder = 3
  end
  object udRow1: TUpDown
    Left = 376
    Top = 32
    Width = 16
    Height = 24
    Min = 0
    Position = 0
    TabOrder = 4
    Wrap = False
    OnClick = udRow1Click
  end
  object udRow2: TUpDown
    Left = 640
    Top = 32
    Width = 16
    Height = 24
    Min = 0
    Position = 0
    TabOrder = 5
    Wrap = False
    OnClick = udRow2Click
  end
  object udCol1: TUpDown
    Left = 24
    Top = 208
    Width = 33
    Height = 16
    Min = 0
    Orientation = udHorizontal
    Position = 0
    TabOrder = 6
    Wrap = False
    OnClick = udCol1Click
  end
  object udCol2: TUpDown
    Left = 424
    Top = 208
    Width = 33
    Height = 16
    Min = 0
    Orientation = udHorizontal
    Position = 0
    TabOrder = 7
    Wrap = False
    OnClick = udCol2Click
  end
  object btnTranspose: TButton
    Left = 88
    Top = 248
    Width = 75
    Height = 25
    Caption = 'Transpose'
    TabOrder = 8
    OnClick = btnTransposeClick
  end
  object btnTrace: TButton
    Left = 208
    Top = 248
    Width = 75
    Height = 25
    Caption = 'Trace'
    TabOrder = 9
    OnClick = btnTraceClick
  end
  object btnSetE: TButton
    Left = 88
    Top = 296
    Width = 75
    Height = 25
    Caption = 'Set to E'
    TabOrder = 10
    OnClick = btnSetEClick
  end
  object btnSetZero: TButton
    Left = 208
    Top = 296
    Width = 75
    Height = 25
    Caption = 'Set to Zero'
    TabOrder = 11
    OnClick = btnSetZeroClick
  end
  object btnDeteminant: TButton
    Left = 88
    Top = 344
    Width = 75
    Height = 25
    Caption = 'Deteminant'
    TabOrder = 12
    OnClick = btnDeteminantClick
  end
  object btnDump: TButton
    Left = 208
    Top = 344
    Width = 75
    Height = 25
    Caption = 'Dump'
    TabOrder = 13
    OnClick = btnDumpClick
  end
  object btnMinor: TButton
    Left = 88
    Top = 400
    Width = 75
    Height = 25
    Caption = 'Minor'
    TabOrder = 14
    OnClick = btnMinorClick
  end
end
