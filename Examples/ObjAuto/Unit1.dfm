object Form1: TForm1
  Left = 195
  Top = 119
  Width = 350
  Height = 257
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 16
    Top = 16
    Width = 313
    Height = 161
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object Button1: TButton
    Left = 96
    Top = 184
    Width = 75
    Height = 25
    Caption = 'ShowCap'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 16
    Top = 184
    Width = 75
    Height = 25
    Caption = 'Add'
    TabOrder = 2
    OnClick = Button2Click
  end
end
