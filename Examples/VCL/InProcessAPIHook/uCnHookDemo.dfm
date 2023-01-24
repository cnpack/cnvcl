object Form1: TForm1
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'CnHook Demo'
  ClientHeight = 275
  ClientWidth = 263
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 19
    Top = 220
    Width = 106
    Height = 25
    Caption = 'MessageBox API'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 19
    Top = 18
    Width = 226
    Height = 196
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object Button2: TButton
    Left = 131
    Top = 220
    Width = 50
    Height = 25
    Caption = 'Hook it'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 187
    Top = 220
    Width = 50
    Height = 25
    Caption = 'Un hook'
    TabOrder = 3
    OnClick = Button3Click
  end
end
