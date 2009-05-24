object Form1: TForm1
  Left = 192
  Top = 107
  Width = 696
  Height = 480
  Caption = 'Raw Input Test'
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
  object lblKbCount: TLabel
    Left = 304
    Top = 32
    Width = 80
    Height = 13
    Caption = 'KeyBoardCount: '
  end
  object btnStart: TButton
    Left = 24
    Top = 24
    Width = 105
    Height = 25
    Caption = 'Start Key Capture'
    TabOrder = 0
    OnClick = btnStartClick
  end
  object Memo1: TMemo
    Left = 24
    Top = 80
    Width = 297
    Height = 345
    TabOrder = 1
  end
  object Memo2: TMemo
    Left = 344
    Top = 80
    Width = 297
    Height = 345
    TabOrder = 2
  end
  object btnStop: TButton
    Left = 152
    Top = 24
    Width = 105
    Height = 25
    Caption = 'Stop Key Capture'
    TabOrder = 3
    OnClick = btnStopClick
  end
  object btnShow: TButton
    Left = 440
    Top = 24
    Width = 129
    Height = 25
    Caption = 'Show KeyBoard Names'
    TabOrder = 4
    OnClick = btnShowClick
  end
end
