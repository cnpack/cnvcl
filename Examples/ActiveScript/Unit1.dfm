object Form1: TForm1
  Left = 454
  Top = 204
  Width = 620
  Height = 370
  Caption = 'CnActiveSctipt Demo'
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
  object lbl1: TLabel
    Left = 8
    Top = 12
    Width = 81
    Height = 13
    Caption = 'Script Language:'
  end
  object lbl2: TLabel
    Left = 8
    Top = 38
    Width = 67
    Height = 13
    Caption = 'Script Source:'
  end
  object cbb1: TComboBox
    Left = 104
    Top = 8
    Width = 153
    Height = 21
    ItemHeight = 13
    TabOrder = 0
  end
  object mmo1: TMemo
    Left = 104
    Top = 40
    Width = 497
    Height = 249
    TabOrder = 1
    WordWrap = False
  end
  object btnRun: TButton
    Left = 272
    Top = 8
    Width = 81
    Height = 25
    Caption = '&Run Script'
    TabOrder = 2
    OnClick = btnRunClick
  end
  object btnDemo1: TButton
    Left = 104
    Top = 304
    Width = 75
    Height = 25
    Caption = 'Demo &1'
    TabOrder = 3
    OnClick = btnDemo1Click
  end
  object btnDemo2: TButton
    Left = 192
    Top = 304
    Width = 75
    Height = 25
    Caption = 'Demo &2'
    TabOrder = 4
    OnClick = btnDemo2Click
  end
  object btnClose: TButton
    Left = 528
    Top = 304
    Width = 75
    Height = 25
    Caption = '&Close'
    TabOrder = 5
    OnClick = btnCloseClick
  end
  object btnExp: TButton
    Left = 360
    Top = 8
    Width = 81
    Height = 25
    Caption = 'Run &Expression'
    TabOrder = 6
    OnClick = btnExpClick
  end
  object btnClean: TButton
    Left = 448
    Top = 8
    Width = 81
    Height = 25
    Caption = 'C&lean Engine'
    TabOrder = 7
    OnClick = btnCleanClick
  end
  object CnActiveScriptWindow1: TCnActiveScriptWindow
    ScriptLanguage = 'VBScript'
    OnError = CnActiveScriptWindow1Error
    CleanBeforeRun = True
    Left = 576
    Top = 8
  end
end
