object FormLang: TFormLang
  Left = 192
  Top = 107
  Width = 979
  Height = 563
  Caption = 'TCnLanguages Test'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lblUtf16: TLabel
    Left = 424
    Top = 24
    Width = 60
    Height = 13
    Caption = 'UTF16 Text:'
  end
  object lblHex: TLabel
    Left = 424
    Top = 80
    Width = 3
    Height = 13
  end
  object mmoLangs: TMemo
    Left = 24
    Top = 24
    Width = 361
    Height = 489
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 0
    WantReturns = False
    WordWrap = False
  end
  object edtText: TEdit
    Left = 424
    Top = 48
    Width = 121
    Height = 21
    TabOrder = 1
    Text = '³Ô·¹Ë¯¾õ'
    OnChange = edtTextChange
  end
  object btnConvert: TButton
    Left = 424
    Top = 128
    Width = 121
    Height = 25
    Caption = 'Convert by CodePages'
    TabOrder = 2
    OnClick = btnConvertClick
  end
  object mmoConvert: TMemo
    Left = 584
    Top = 24
    Width = 369
    Height = 489
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 3
    WantReturns = False
    WordWrap = False
  end
end
