object Form1: TForm1
  Left = 213
  Top = 145
  Width = 357
  Height = 265
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 32
    Top = 32
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 0
  end
  object Edit1: TEdit
    Left = 8
    Top = 8
    Width = 121
    Height = 21
    TabOrder = 1
    Text = 'Edit1'
  end
  object CnDragResizer1: TCnDragResizer
    Control = Button1
    ShowBounds = True
    Left = 48
    Top = 16
  end
  object CnDragResizer2: TCnDragResizer
    Control = Edit1
    KeepInParent = False
    ShowBounds = False
    Left = 80
    Top = 16
  end
end
