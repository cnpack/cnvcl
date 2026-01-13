object FormTLSServer: TFormTLSServer
  Left = 244
  Top = 152
  Width = 600
  Height = 445
  Caption = 'TLSServer'
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
  object edtIP: TEdit
    Left = 16
    Top = 16
    Width = 160
    Height = 21
    TabOrder = 0
    Text = '127.0.0.1'
  end
  object edtPort: TEdit
    Left = 192
    Top = 16
    Width = 80
    Height = 21
    TabOrder = 1
    Text = '8443'
  end
  object btnOpen: TButton
    Left = 288
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Open'
    TabOrder = 2
    OnClick = btnOpenClick
  end
  object btnClose: TButton
    Left = 376
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Close'
    TabOrder = 3
    OnClick = btnCloseClick
  end
  object mmoLog: TMemo
    Left = 16
    Top = 56
    Width = 560
    Height = 336
    TabOrder = 4
  end
end
