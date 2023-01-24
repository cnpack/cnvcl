object FormTCPClient: TFormTCPClient
  Left = 277
  Top = 183
  BorderStyle = bsDialog
  Caption = 'TCP Client'
  ClientHeight = 630
  ClientWidth = 726
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
  object lblIP: TLabel
    Left = 24
    Top = 24
    Width = 25
    Height = 13
    Caption = 'Host:'
  end
  object lblPort: TLabel
    Left = 24
    Top = 56
    Width = 19
    Height = 13
    Caption = 'Port'
  end
  object edtHost: TEdit
    Left = 56
    Top = 24
    Width = 121
    Height = 21
    TabOrder = 0
    Text = '127.0.0.1'
  end
  object edtPort: TEdit
    Left = 56
    Top = 56
    Width = 121
    Height = 21
    TabOrder = 1
    Text = '30841'
  end
  object btnOpen: TButton
    Left = 208
    Top = 24
    Width = 97
    Height = 57
    Caption = 'Connect'
    TabOrder = 2
    OnClick = btnOpenClick
  end
  object mmoResult: TMemo
    Left = 24
    Top = 96
    Width = 673
    Height = 505
    ReadOnly = True
    TabOrder = 3
  end
  object mmoContent: TMemo
    Left = 376
    Top = 24
    Width = 185
    Height = 57
    Lines.Strings = (
      'Test!')
    TabOrder = 4
  end
  object btnSend: TButton
    Left = 584
    Top = 24
    Width = 113
    Height = 57
    Caption = 'Send'
    TabOrder = 5
    OnClick = btnSendClick
  end
end
