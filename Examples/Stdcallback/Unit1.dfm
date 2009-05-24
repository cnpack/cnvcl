object Form1: TForm1
  Left = 423
  Top = 294
  BorderStyle = bsDialog
  Caption = 'StdCall Method Callback Test'
  ClientHeight = 75
  ClientWidth = 301
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
  object btn1: TButton
    Left = 24
    Top = 24
    Width = 129
    Height = 25
    Caption = 'Register Callback'
    TabOrder = 0
    OnClick = btn1Click
  end
  object Button1: TButton
    Left = 192
    Top = 24
    Width = 89
    Height = 25
    Caption = 'Call it !'
    TabOrder = 1
    OnClick = Button1Click
  end
end
