object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 562
  ClientWidth = 933
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object mmo1: TMemo
    Left = 8
    Top = 8
    Width = 449
    Height = 513
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object mmo2: TMemo
    Left = 472
    Top = 8
    Width = 449
    Height = 513
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object btnTest: TButton
    Left = 8
    Top = 527
    Width = 75
    Height = 25
    Caption = 'Test'
    TabOrder = 2
    OnClick = btnTestClick
  end
end
