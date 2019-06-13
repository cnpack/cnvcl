object FormMatrix: TFormMatrix
  Left = 199
  Top = 156
  Width = 1099
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
    Left = 720
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
  object btnREqu: TSpeedButton
    Left = 720
    Top = 136
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
    OnClick = btnREquClick
  end
  object StringGrid1: TStringGrid
    Left = 24
    Top = 32
    Width = 257
    Height = 169
    DefaultColWidth = 48
    FixedCols = 0
    RowCount = 6
    FixedRows = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing]
    TabOrder = 0
  end
  object StaticText1: TStaticText
    Left = 296
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
    Left = 336
    Top = 32
    Width = 361
    Height = 169
    ColCount = 3
    DefaultColWidth = 84
    FixedCols = 0
    FixedRows = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing]
    TabOrder = 2
  end
  object StringGridR: TStringGrid
    Left = 760
    Top = 32
    Width = 313
    Height = 169
    ColCount = 1
    DefaultColWidth = 84
    FixedCols = 0
    RowCount = 1
    FixedRows = 0
    TabOrder = 3
  end
  object udRow1: TUpDown
    Left = 288
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
    Left = 704
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
    Left = 336
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
  object grpInt: TGroupBox
    Left = 80
    Top = 232
    Width = 273
    Height = 281
    Caption = 'Int Matrix'
    TabOrder = 8
    object btnTranspose: TButton
      Left = 32
      Top = 32
      Width = 75
      Height = 25
      Caption = 'Transpose'
      TabOrder = 0
      OnClick = btnTransposeClick
    end
    object btnTrace: TButton
      Left = 152
      Top = 32
      Width = 75
      Height = 25
      Caption = 'Trace'
      TabOrder = 1
      OnClick = btnTraceClick
    end
    object btnSetE: TButton
      Left = 32
      Top = 80
      Width = 75
      Height = 25
      Caption = 'Set to E'
      TabOrder = 2
      OnClick = btnSetEClick
    end
    object btnSetZero: TButton
      Left = 152
      Top = 80
      Width = 75
      Height = 25
      Caption = 'Set to Zero'
      TabOrder = 3
      OnClick = btnSetZeroClick
    end
    object btnDeteminant: TButton
      Left = 32
      Top = 128
      Width = 75
      Height = 25
      Caption = 'Deteminant'
      TabOrder = 4
      OnClick = btnDeteminantClick
    end
    object btnDump: TButton
      Left = 152
      Top = 128
      Width = 75
      Height = 25
      Caption = 'Dump'
      TabOrder = 5
      OnClick = btnDumpClick
    end
    object btnMinor: TButton
      Left = 32
      Top = 176
      Width = 75
      Height = 25
      Caption = 'Minor'
      TabOrder = 6
      OnClick = btnMinorClick
    end
    object btnAdjoint: TButton
      Left = 152
      Top = 176
      Width = 75
      Height = 25
      Caption = 'Adjoint'
      TabOrder = 7
      OnClick = btnAdjointClick
    end
  end
  object grpRational: TGroupBox
    Left = 496
    Top = 232
    Width = 305
    Height = 281
    Caption = 'Rational Matrix'
    TabOrder = 9
    object btnRTranspose: TButton
      Left = 32
      Top = 32
      Width = 75
      Height = 25
      Caption = 'Transpose'
      TabOrder = 0
      OnClick = btnRTransposeClick
    end
    object btnRTrace: TButton
      Left = 152
      Top = 32
      Width = 75
      Height = 25
      Caption = 'Trace'
      TabOrder = 1
      OnClick = btnRTraceClick
    end
    object btnRSetE: TButton
      Left = 32
      Top = 80
      Width = 75
      Height = 25
      Caption = 'Set to E'
      TabOrder = 2
      OnClick = btnRSetEClick
    end
    object btnRSetZero: TButton
      Left = 152
      Top = 80
      Width = 75
      Height = 25
      Caption = 'Set to Zero'
      TabOrder = 3
      OnClick = btnRSetZeroClick
    end
    object btnRDump: TButton
      Left = 152
      Top = 128
      Width = 75
      Height = 25
      Caption = 'Dump'
      TabOrder = 4
      OnClick = btnRDumpClick
    end
    object btnRDeter: TButton
      Left = 32
      Top = 128
      Width = 75
      Height = 25
      Caption = 'Deteminant'
      TabOrder = 5
      OnClick = btnRDeterClick
    end
    object btnRMinor: TButton
      Left = 32
      Top = 176
      Width = 75
      Height = 25
      Caption = 'Minor'
      TabOrder = 6
      OnClick = btnRMinorClick
    end
    object btnRAdj: TButton
      Left = 152
      Top = 176
      Width = 75
      Height = 25
      Caption = 'Adjoint'
      TabOrder = 7
      OnClick = btnRAdjClick
    end
    object btnInverse: TButton
      Left = 32
      Top = 224
      Width = 75
      Height = 25
      Caption = 'Inverse'
      TabOrder = 8
      OnClick = btnInverseClick
    end
  end
end
