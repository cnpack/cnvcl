object FormParseBer: TFormParseBer
  Left = 294
  Top = 161
  Width = 645
  Height = 413
  Caption = 'BER Format Parser'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object mmoResult: TMemo
    Left = 8
    Top = 64
    Width = 337
    Height = 313
    ReadOnly = True
    TabOrder = 0
  end
  object btnParse: TButton
    Left = 552
    Top = 24
    Width = 75
    Height = 25
    Caption = 'Parse'
    TabOrder = 1
    OnClick = btnParseClick
  end
  object tv1: TTreeView
    Left = 360
    Top = 64
    Width = 265
    Height = 313
    Indent = 19
    TabOrder = 2
  end
end
