object CnMultiLangMergeFrm: TCnMultiLangMergeFrm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'CnPack Multi-Language Items Merger'
  ClientHeight = 443
  ClientWidth = 584
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object lbl1: TLabel
    Left = 8
    Top = 8
    Width = 117
    Height = 13
    Caption = 'Source Language Items:'
  end
  object lbl2: TLabel
    Left = 296
    Top = 13
    Width = 170
    Height = 13
    Caption = 'Translated Target Language Items:'
  end
  object lbl3: TLabel
    Left = 8
    Top = 205
    Width = 155
    Height = 13
    Caption = 'Merged Target Language Items:'
  end
  object mmo1: TMemo
    Left = 8
    Top = 32
    Width = 273
    Height = 161
    ScrollBars = ssBoth
    TabOrder = 0
    WordWrap = False
  end
  object mmo2: TMemo
    Left = 296
    Top = 32
    Width = 273
    Height = 161
    ScrollBars = ssBoth
    TabOrder = 1
    WordWrap = False
  end
  object mmo3: TMemo
    Left = 8
    Top = 224
    Width = 561
    Height = 177
    ScrollBars = ssBoth
    TabOrder = 2
    WordWrap = False
  end
  object btnMerge: TButton
    Left = 208
    Top = 407
    Width = 75
    Height = 25
    Caption = 'Merge'
    TabOrder = 3
    OnClick = btnMergeClick
  end
  object btnCopy: TButton
    Left = 296
    Top = 407
    Width = 75
    Height = 25
    Caption = 'Copy'
    TabOrder = 4
    OnClick = btnCopyClick
  end
end
