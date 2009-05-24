object Form1: TForm1
  Left = 405
  Top = 243
  Width = 339
  Height = 377
  Caption = 'Simple Test for Iocp Simple MemPool'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object btn1: TButton
    Left = 31
    Top = 24
    Width = 75
    Height = 25
    Caption = 'Rent Memory'
    TabOrder = 0
    OnClick = btn1Click
  end
  object btnFree: TButton
    Left = 224
    Top = 24
    Width = 83
    Height = 25
    Caption = 'Return Memory'
    TabOrder = 1
    OnClick = btnFreeClick
  end
  object btnDisp: TButton
    Left = 128
    Top = 24
    Width = 75
    Height = 25
    Caption = 'Show Mem'
    TabOrder = 2
    OnClick = btnDispClick
  end
  object mmo1: TMemo
    Left = 32
    Top = 72
    Width = 275
    Height = 249
    ScrollBars = ssVertical
    TabOrder = 3
  end
  object cncpsmplmpl1: TCnIocpSimpleMemPool
    MemorySize = 1024
    Threshold = 20
    Left = 112
    Top = 32
  end
end
