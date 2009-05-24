object FrmMain: TFrmMain
  Left = 286
  Top = 147
  Width = 442
  Height = 341
  Caption = 'Linked List Demo'
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
  object Memo1: TMemo
    Left = 0
    Top = 0
    Width = 434
    Height = 249
    Align = alTop
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object Button1: TButton
    Left = 339
    Top = 266
    Width = 75
    Height = 25
    Caption = 'Multi-Thread'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 15
    Top = 266
    Width = 75
    Height = 25
    Caption = 'Add'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 96
    Top = 266
    Width = 75
    Height = 25
    Caption = 'Delete'
    TabOrder = 3
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 177
    Top = 266
    Width = 75
    Height = 25
    Caption = 'PrintList'
    TabOrder = 4
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 258
    Top = 266
    Width = 75
    Height = 25
    Caption = 'Exchange'
    TabOrder = 5
    OnClick = Button5Click
  end
end
