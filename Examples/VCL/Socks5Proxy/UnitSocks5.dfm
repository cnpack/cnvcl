object FormSocks5: TFormSocks5
  Left = 356
  Top = 170
  Width = 1138
  Height = 718
  Caption = 'Simple Socks5 Proxy'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lblLocalIP: TLabel
    Left = 24
    Top = 24
    Width = 42
    Height = 13
    Caption = 'Local IP:'
  end
  object lblProxyPort: TLabel
    Left = 24
    Top = 56
    Width = 46
    Height = 13
    Caption = 'Prox Port:'
  end
  object edtLocalIP: TEdit
    Left = 88
    Top = 24
    Width = 121
    Height = 21
    TabOrder = 0
    Text = '127.0.0.1'
  end
  object edtPort: TEdit
    Left = 88
    Top = 56
    Width = 121
    Height = 21
    TabOrder = 1
    Text = '30841'
  end
  object btnProxy: TButton
    Left = 232
    Top = 24
    Width = 75
    Height = 49
    Caption = 'Proxy!'
    TabOrder = 2
    OnClick = btnProxyClick
  end
  object lstClients: TListBox
    Left = 24
    Top = 104
    Width = 281
    Height = 554
    Anchors = [akLeft, akTop, akBottom]
    ItemHeight = 13
    TabOrder = 3
  end
  object mmoLog: TMemo
    Left = 336
    Top = 24
    Width = 761
    Height = 634
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 4
  end
  object tmrClient: TTimer
    OnTimer = tmrClientTimer
    Left = 56
    Top = 136
  end
end
