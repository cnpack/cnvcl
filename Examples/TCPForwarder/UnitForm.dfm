object FormForwarder: TFormForwarder
  Left = 192
  Top = 107
  Width = 608
  Height = 636
  Caption = 'TCP Forwarder - 端口转发，注意不能做代理因为缺乏 Host 等的处理'
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
    Width = 42
    Height = 13
    Caption = 'Local IP:'
  end
  object lblPort: TLabel
    Left = 24
    Top = 56
    Width = 48
    Height = 13
    Caption = 'Local Port'
  end
  object lblRemoteHost: TLabel
    Left = 368
    Top = 24
    Width = 65
    Height = 13
    Caption = 'Remote Host:'
  end
  object lblRemotePort: TLabel
    Left = 368
    Top = 56
    Width = 62
    Height = 13
    Caption = 'Remote Port:'
  end
  object edtLocalIP: TEdit
    Left = 88
    Top = 24
    Width = 121
    Height = 21
    TabOrder = 0
    Text = '127.0.0.1'
  end
  object edtLocalPort: TEdit
    Left = 88
    Top = 56
    Width = 121
    Height = 21
    TabOrder = 1
    Text = '30840'
  end
  object btnOpen: TButton
    Left = 240
    Top = 24
    Width = 105
    Height = 57
    Caption = 'Forward to:'
    TabOrder = 2
    OnClick = btnOpenClick
  end
  object edtRemoteHost: TEdit
    Left = 448
    Top = 24
    Width = 121
    Height = 21
    TabOrder = 3
    Text = 'www.baidu.com'
  end
  object edtRemotePort: TEdit
    Left = 448
    Top = 56
    Width = 121
    Height = 21
    TabOrder = 4
    Text = '80'
  end
  object mmoResult: TMemo
    Left = 24
    Top = 104
    Width = 545
    Height = 473
    TabOrder = 5
  end
end
