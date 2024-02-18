object FormWatermark: TFormWatermark
  Left = 192
  Top = 107
  Width = 979
  Height = 563
  Caption = 'Watermark Test - NOT Ready'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object img1: TImage
    Left = 16
    Top = 56
    Width = 305
    Height = 297
  end
  object btnOpenFile: TButton
    Left = 16
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Open Image'
    TabOrder = 0
    OnClick = btnOpenFileClick
  end
  object dlgOpen1: TOpenDialog
    Filter = 'JPG|*.jpg'
    Left = 96
    Top = 16
  end
end
