object Form4: TForm4
  Left = 463
  Top = 363
  BorderStyle = bsDialog
  Caption = 'Generate Text'
  ClientHeight = 100
  ClientWidth = 195
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 8
    Top = 65
    Width = 75
    Height = 25
    Caption = 'Radom'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Edit1: TEdit
    Left = 8
    Top = 38
    Width = 177
    Height = 21
    TabOrder = 1
    Text = '10000000'
  end
  object Button2: TButton
    Left = 110
    Top = 65
    Width = 75
    Height = 25
    Caption = 'Regular'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Edit2: TEdit
    Left = 8
    Top = 8
    Width = 177
    Height = 21
    TabOrder = 3
    Text = 'C:\test.txt'
  end
end
