object FormPDF: TFormPDF
  Left = 193
  Top = 108
  Width = 1130
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
    Left = 328
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
  object mmoPDF: TMemo
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
  object btnImages: TButton
    Left = 824
    Top = 16
    Width = 137
    Height = 25
    Caption = 'Images To PDF'
    TabOrder = 4
    OnClick = btnImagesClick
  end
  object btnAddJPG: TButton
    Left = 1008
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Add JPGs'
    TabOrder = 5
    OnClick = btnAddJPGClick
  end
  object lstJpegs: TListBox
    Left = 824
    Top = 56
    Width = 273
    Height = 457
    ItemHeight = 13
    TabOrder = 6
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
