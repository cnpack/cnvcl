object DancingLinksForm: TDancingLinksForm
  Left = 192
  Top = 130
  Width = 928
  Height = 480
  Caption = 'Test Dancing Links Form - Left/Right Click Cell to Add/Delete.'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object lblRow: TLabel
    Left = 88
    Top = 36
    Width = 22
    Height = 13
    Caption = 'Row'
  end
  object lblCol: TLabel
    Left = 168
    Top = 36
    Width = 35
    Height = 13
    Caption = 'Column'
  end
  object Grid: TStringGrid
    Left = 32
    Top = 72
    Width = 841
    Height = 345
    ColCount = 2
    DefaultColWidth = 44
    RowCount = 2
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goDrawFocusSelected]
    TabOrder = 0
    OnMouseDown = GridMouseDown
    OnMouseUp = GridMouseUp
    OnSelectCell = GridSelectCell
  end
  object edtRow: TEdit
    Left = 32
    Top = 32
    Width = 33
    Height = 21
    TabOrder = 1
    Text = '10'
  end
  object udRow: TUpDown
    Left = 65
    Top = 32
    Width = 16
    Height = 21
    Associate = edtRow
    Min = 0
    Position = 10
    TabOrder = 2
    Wrap = False
  end
  object edtCol: TEdit
    Left = 112
    Top = 32
    Width = 33
    Height = 21
    TabOrder = 3
    Text = '12'
  end
  object udCol: TUpDown
    Left = 145
    Top = 32
    Width = 16
    Height = 21
    Associate = edtCol
    Min = 0
    Position = 12
    TabOrder = 4
    Wrap = False
  end
  object btnCreate: TButton
    Left = 216
    Top = 32
    Width = 75
    Height = 25
    Caption = 'Create'
    TabOrder = 5
    OnClick = btnCreateClick
  end
  object stat1: TStatusBar
    Left = 0
    Top = 423
    Width = 912
    Height = 19
    Panels = <
      item
        Width = 400
      end
      item
        Width = 250
      end
      item
        Width = 50
      end>
    SimplePanel = False
  end
  object btnExpandRow: TButton
    Left = 392
    Top = 32
    Width = 75
    Height = 25
    Caption = 'Expand Row'
    TabOrder = 7
    OnClick = btnExpandRowClick
  end
  object btnExpandCol: TButton
    Left = 488
    Top = 32
    Width = 75
    Height = 25
    Caption = 'Expand Col'
    TabOrder = 8
    OnClick = btnExpandColClick
  end
  object btnDump: TButton
    Left = 304
    Top = 32
    Width = 75
    Height = 25
    Caption = 'Dump'
    TabOrder = 9
    OnClick = btnDumpClick
  end
end
