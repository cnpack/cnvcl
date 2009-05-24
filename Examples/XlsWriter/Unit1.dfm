object Form1: TForm1
  Left = 183
  Top = 268
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'CnXlsWriter Test'
  ClientHeight = 227
  ClientWidth = 672
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 345
    Top = 184
    Width = 105
    Height = 25
    Caption = 'Write a.xls'
    TabOrder = 0
    OnClick = Button1Click
  end
  object StringGrid1: TStringGrid
    Left = 31
    Top = 16
    Width = 610
    Height = 154
    ColCount = 6
    DefaultColWidth = 100
    RowCount = 6
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing]
    TabOrder = 1
  end
  object CheckBox1: TCheckBox
    Left = 222
    Top = 188
    Width = 97
    Height = 17
    Caption = 'ADOCompatible'
    TabOrder = 2
  end
end
