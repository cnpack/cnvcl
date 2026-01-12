object FormTCPServer: TFormTCPServer
  Left = 192
  Top = 107
  BorderStyle = bsDialog
  Caption = 'Threading TCP Server'
  ClientHeight = 629
  ClientWidth = 727
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
    Width = 13
    Height = 13
    Caption = 'IP:'
  end
  object lblPort: TLabel
    Left = 24
    Top = 56
    Width = 19
    Height = 13
    Caption = 'Port'
  end
  object edtIP: TEdit
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
    Caption = 'Open'
    TabOrder = 2
    OnClick = btnOpenClick
  end
  object btnOpenTLS: TButton
    Left = 320
    Top = 24
    Width = 97
    Height = 57
    Caption = 'Start TLS Server'
    TabOrder = 4
    OnClick = btnOpenTLSClick
  end
  object mmoResult: TMemo
    Left = 24
    Top = 96
    Width = 673
    Height = 505
    ReadOnly = True
    TabOrder = 3
  end
end
