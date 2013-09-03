object Form1: TForm1
  Left = 104
  Top = 73
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'CnSearch Demo'
  ClientHeight = 309
  ClientWidth = 474
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
  object Memo1: TMemo
    Left = 26
    Top = 18
    Width = 423
    Height = 198
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object Button1: TButton
    Left = 26
    Top = 230
    Width = 85
    Height = 25
    Caption = '文件标志搜索'
    TabOrder = 1
    OnClick = Button1Click
  end
end
