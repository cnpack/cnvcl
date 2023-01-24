object Form1: TForm1
  Left = 171
  Top = 120
  Width = 463
  Height = 348
  Caption = 'ColorGrid Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 344
    Top = 189
    Width = 57
    Height = 13
    Caption = 'Select Color'
  end
  object Button1: TButton
    Left = 344
    Top = 72
    Width = 75
    Height = 25
    Caption = 'Next Set'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Edit1: TEdit
    Left = 344
    Top = 208
    Width = 57
    Height = 21
    Color = clWhite
    TabOrder = 2
  end
  object Button2: TButton
    Left = 344
    Top = 119
    Width = 75
    Height = 25
    Caption = 'previous Set'
    TabOrder = 1
    OnClick = Button2Click
  end
  object CnColorGrid1: TCnColorGrid
    Left = 56
    Top = 16
    Width = 195
    Height = 291
    ColCount = 12
    DefaultColWidth = 15
    DefaultRowHeight = 15
    FixedCols = 0
    RowCount = 18
    FixedRows = 0
    Options = [goFixedVertLine, goFixedHorzLine]
    ScrollBars = ssNone
    TabOrder = 3
    OnSelectCell = CnColorGrid1SelectCell
    CustomRowCount = 10
    CustomColCount = 10
  end
end
