object FormPDF: TFormPDF
  Left = 193
  Top = 108
  Width = 979
  Height = 563
  Caption = 'PDF'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object btnGenSimple: TButton
    Left = 480
    Top = 16
    Width = 113
    Height = 25
    Caption = 'Generate Simple'
    TabOrder = 0
    OnClick = btnGenSimpleClick
  end
  object btnParsePDFToken: TButton
    Left = 24
    Top = 16
    Width = 105
    Height = 25
    Caption = 'Parse PDF Tokens'
    TabOrder = 1
    OnClick = btnParsePDFTokenClick
  end
  object mmoPDFToken: TMemo
    Left = 24
    Top = 56
    Width = 793
    Height = 457
    ScrollBars = ssVertical
    TabOrder = 2
  end
  object btnParsePDFStructure: TButton
    Left = 160
    Top = 16
    Width = 113
    Height = 25
    Caption = 'Parse PDF Structure'
    TabOrder = 3
    OnClick = btnParsePDFStructureClick
  end
  object dlgSave1: TSaveDialog
    Left = 448
    Top = 8
  end
  object dlgOpen1: TOpenDialog
    Left = 272
    Top = 16
  end
end
