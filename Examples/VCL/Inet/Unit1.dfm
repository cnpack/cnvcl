object Form1: TForm1
  Left = 283
  Top = 161
  Width = 553
  Height = 457
  Caption = 'Form1'
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
  object lbl1: TLabel
    Left = 8
    Top = 11
    Width = 25
    Height = 13
    Caption = 'URL:'
  end
  object mmo1: TMemo
    Left = 8
    Top = 72
    Width = 529
    Height = 321
    ScrollBars = ssBoth
    TabOrder = 4
  end
  object cbb1: TComboBox
    Left = 40
    Top = 8
    Width = 497
    Height = 21
    ItemHeight = 13
    TabOrder = 0
    Items.Strings = (
      'http://www.cnpack.org/index.php?lang=en'
      'ftp://user:pass@ftp.anyhost.com/dir/filename.zip')
  end
  object btn1: TButton
    Left = 40
    Top = 40
    Width = 73
    Height = 21
    Caption = 'Get &Text'
    TabOrder = 1
    OnClick = btn1Click
  end
  object btn2: TButton
    Left = 120
    Top = 40
    Width = 75
    Height = 21
    Caption = 'Save To &File'
    TabOrder = 2
    OnClick = btn2Click
  end
  object pb1: TProgressBar
    Left = 208
    Top = 42
    Width = 249
    Height = 16
    Min = 0
    Max = 100
    TabOrder = 3
  end
  object btn3: TButton
    Left = 464
    Top = 400
    Width = 73
    Height = 21
    Caption = '&Close'
    TabOrder = 5
  end
  object btn4: TButton
    Left = 464
    Top = 40
    Width = 75
    Height = 21
    Caption = '&Abort'
    TabOrder = 6
    OnClick = btn4Click
  end
  object dlgSave1: TSaveDialog
    Left = 8
    Top = 384
  end
end
